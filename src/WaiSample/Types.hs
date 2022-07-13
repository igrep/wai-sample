{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

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
    :: (ToRawResponse resSpec, FromRawResponse resSpec)
    => String -> Method -> RoutingTable a -> (a -> IO (ResponseObject resSpec)) -> Handler


data WithStatus status resTyp = WithStatus status resTyp deriving (Eq, Show, Lift)

-- NOTE: Current implementation has a problem that for example `WithStatus 404 (WithStatus 500 PlainText)` ignores 500. But I'll ignore the problem
instance IsStatusCode status => HasStatusCode (WithStatus status resTyp) where
  statusCodes _ = [toStatusCode (Proxy :: Proxy status)]

instance (Lift status, HasContentTypes resTyp) => HasContentTypes (WithStatus status resTyp) where
  contentTypes _ = contentTypes (Proxy :: Proxy resTyp)


instance (Typeable status, Lift status, IsStatusCode status, ToRawResponse (resTyp, resObj)) => ToRawResponse (WithStatus status resTyp, resObj) where
  toRawResponse mediaType res = do
    rr <- toRawResponse @(resTyp, resObj) mediaType res
    return $ RawResponse (Just (toStatusCode (Proxy :: Proxy status))) (rawBody rr)


instance (Typeable status, Lift status, IsStatusCode status, FromRawResponse (resTyp, resObj)) => FromRawResponse (WithStatus status resTyp, resObj) where
  fromRawResponse mediaType rr = do
    resObj <- fromRawResponse @(resTyp, resObj) mediaType rr
    rawSt <- maybe (fail "Unexpected status code") return $ rawStatusCode rr
    status <- maybe (fail "Unexpected status code") return $ fromStatusCode rawSt
    return resObj
