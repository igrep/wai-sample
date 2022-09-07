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
    ::
      ( ToRawResponse resSpec
      , FromRawResponse resSpec
      , Typeable resSpec
      , Typeable (ResponseObject resSpec)
      , HasStatusCode (ResponseType resSpec)
      , HasContentTypes (ResponseType resSpec)
      )
    => Proxy resSpec -> String -> Method -> RoutingTable a -> (a -> IO (ResponseObject resSpec)) -> Handler


data WithStatus status resTyp = WithStatus status resTyp deriving (Eq, Show, Lift)

-- NOTE: Current implementation has a problem that for example `WithStatus 404 (WithStatus 500 PlainText)` ignores 500. But I'll ignore the problem
instance IsStatusCode status => HasStatusCode (WithStatus status resTyp) where
  statusCodes = [NonDefaultStatus $ toStatusCode (Proxy :: Proxy status)]

instance (Lift status, HasContentTypes resTyp) => HasContentTypes (WithStatus status resTyp) where
  contentTypes = contentTypes @resTyp


instance
  (Typeable status
  , Lift status
  , IsStatusCode status
  , HasContentTypes resTyp
  , Typeable resObj
  , ToRawResponse (resTyp, resObj)
  ) => ToRawResponse (WithStatus status resTyp, resObj) where
  toRawResponse mediaType res = do
    rr <- toRawResponse @(resTyp, resObj) mediaType res
    return $ RawResponse (NonDefaultStatus (toStatusCode (Proxy :: Proxy status))) (rawBody rr)


instance
  (Typeable status
  , Lift status
  , IsStatusCode status
  , HasContentTypes resTyp
  , Typeable resObj
  , FromRawResponse (resTyp, resObj)
  ) => FromRawResponse (WithStatus status resTyp, resObj) where
  fromRawResponse mediaType rr = do
    resObj <- fromRawResponse @(resTyp, resObj) mediaType rr
    rawSt <-
      case rawStatusCode rr of
          DefaultStatus       -> fail "Unexpected status code"
          NonDefaultStatus st -> return st
    _ <- maybe (fail "Unexpected status code") return $ fromStatusCode @status rawSt
    return resObj
