{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module WaiSample.Types
  ( RoutingTable (..)
  , Handler (..)
  , WithStatus (..)
  , module WaiSample.Types.ContentTypes
  , module WaiSample.Types.Response
  , module WaiSample.Types.Response.Sum
  , module WaiSample.Types.Status
  ) where

import           Data.Proxy                   (Proxy (Proxy))
import qualified Data.Text                    as T
import           Data.Typeable                (Typeable)
import           Language.Haskell.TH.Syntax   (Lift)
import           Network.HTTP.Types.Method    (Method)
import           Web.HttpApiData              (FromHttpApiData, ToHttpApiData)

import           WaiSample.Types.ContentTypes
import           WaiSample.Types.Response
import           WaiSample.Types.Response.Sum
import           WaiSample.Types.Status


-- TODO: Rename into Route?
data RoutingTable a where
  LiteralPath :: T.Text -> RoutingTable T.Text
  -- | '<$>'
  FmapPath :: (a -> b) -> RoutingTable a -> RoutingTable b
  PurePath :: a -> RoutingTable a
  -- | '<*>'
  ApPath :: RoutingTable (a -> b) -> RoutingTable a -> RoutingTable b
  ParsedPath :: (ToHttpApiData a, FromHttpApiData a, Typeable a) => Proxy a -> RoutingTable a

instance Functor RoutingTable where
  fmap = FmapPath

instance Applicative RoutingTable where
  pure = PurePath
  (<*>) = ApPath

data Handler where
  Handler
    :: (ToRawResponse resTyp resObj, FromRawResponse resTyp resObj)
    => String -> Method -> RoutingTable a -> resTyp -> (a -> IO resObj) -> Handler
    --                                                          ^^^^^^
    --                                                          Text
    --                                                      (Response Status404 Text)


data WithStatus status resTyp = WithStatus status resTyp deriving (Eq, Show, Lift)

-- NOTE: Current implementation has a problem that for example `WithStatus 404 (WithStatus 500 PlainText)` ignores 500. But I'll ignore the problem
instance IsStatusCode status => HasStatusCode (WithStatus status resTyp) where
  statusCodes _ = [toStatusCode (Proxy :: Proxy status)]

instance (Lift status, HasContentTypes resTyp) => HasContentTypes (WithStatus status resTyp) where
  contentTypes _ = contentTypes (Proxy :: Proxy resTyp)


instance (Typeable status, Lift status, IsStatusCode status, ToRawResponse resTyp resObj) => ToRawResponse (WithStatus status resTyp) (Response status resObj) where
  toRawResponse mediaType _ res = do
    rr <- toRawResponse mediaType (Proxy :: Proxy resTyp) (bodyObject res)
    return $ RawResponse (Just (toStatusCode (Proxy :: Proxy status))) (rawBody rr)


instance (Typeable status, Lift status, IsStatusCode status, FromRawResponse resTyp resObj) => FromRawResponse (WithStatus status resTyp) (Response status resObj) where
  fromRawResponse mediaType _ rr = do
    resObj <- fromRawResponse mediaType (Proxy :: Proxy resTyp) rr
    rawSt <- maybe (fail "Unexpected status code") return $ rawStatusCode rr
    status <- maybe (fail "Unexpected status code") return $ fromStatusCode rawSt
    return $ Response status resObj
