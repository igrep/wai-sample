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
  ( Route (..)
  , Handler (..)
  , Responder
  , SimpleResponder
  , EndpointOptions (..)
  , options
  , RequestInfo (..)
  , RequestHeadersCodec (..)
  , HasRequestHeadersCodec (..)
  , requestHeader
  , WithStatus (..)
  , module WaiSample.Types.ContentTypes
  , module WaiSample.Types.Response.Headers
  , module WaiSample.Types.Response
  , module WaiSample.Types.Response.Sum
  , module WaiSample.Types.Status
  ) where

import           Data.Proxy                       (Proxy (Proxy))
import qualified Data.Text                        as T
import           Data.Typeable                    (Typeable)
import           Language.Haskell.TH.Syntax       (Lift)
import           Network.HTTP.Types.Method        (Method)
import           Web.HttpApiData                  (FromHttpApiData,
                                                   ToHttpApiData)

import           Control.Applicative              (Alternative (empty, (<|>)),
                                                   optional)
import           Data.Void                        (Void)
import           Network.HTTP.Types               (HeaderName)
import           WaiSample.Types.ContentTypes
import           WaiSample.Types.Response
import           WaiSample.Types.Response.Headers
import           WaiSample.Types.Response.Sum
import           WaiSample.Types.Status


data Route a where
  LiteralPath :: T.Text -> Route T.Text

  -- TODO: Rename into FmapRoute, PureRoute and ApRoute.
  -- | '<$>'
  FmapPath :: (a -> b) -> Route a -> Route b
  PurePath :: a -> Route a
  -- | '<*>'
  ApPath :: Route (a -> b) -> Route a -> Route b
  ParsedPath :: (ToHttpApiData a, FromHttpApiData a, Typeable a) => Route a

instance Functor Route where
  fmap = FmapPath

instance Applicative Route where
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
      , Typeable h
      , HasRequestHeadersCodec h
      )
    => Proxy resSpec
    -> String
    -> Method
    -> Route a
    -> EndpointOptions h
    -> Responder a h (ResponseObject resSpec)
    -> Handler


type Responder a h resObj = a -> RequestInfo h -> IO resObj

type SimpleResponder a resObj = a -> IO resObj


newtype EndpointOptions h = EndpointOptions
  { headersType :: Proxy h
  }

options :: EndpointOptions Void
options = EndpointOptions
  { headersType = Proxy
  }

newtype RequestInfo h = RequestInfo { requestHeadersValue :: h }

data RequestHeadersCodec h where
  RequestHeader :: (ToHttpApiData h, FromHttpApiData h, Typeable h) => HeaderName -> RequestHeadersCodec h

  EmptyRequestHeader :: RequestHeadersCodec h
  -- | '<$>'
  FmapRequestHeader :: (h -> i) -> RequestHeadersCodec h -> RequestHeadersCodec i
  PureRequestHeader :: h -> RequestHeadersCodec h
  -- | '<*>'
  ApRequestHeader :: RequestHeadersCodec (h -> i) -> RequestHeadersCodec h -> RequestHeadersCodec i
  AltRequestHeader :: RequestHeadersCodec h -> RequestHeadersCodec h -> RequestHeadersCodec h

instance Functor RequestHeadersCodec where
  fmap = FmapRequestHeader

instance Applicative RequestHeadersCodec where
  pure = PureRequestHeader
  (<*>) = ApRequestHeader

instance Alternative RequestHeadersCodec where
  empty = EmptyRequestHeader
  (<|>) = AltRequestHeader

class HasRequestHeadersCodec h where
  requestHeadersCodec :: RequestHeadersCodec h

instance HasRequestHeadersCodec h => HasRequestHeadersCodec (Maybe h) where
  requestHeadersCodec = optional requestHeadersCodec

instance HasRequestHeadersCodec Void where
  requestHeadersCodec = EmptyRequestHeader


requestHeader :: (ToHttpApiData h, FromHttpApiData h, Typeable h) => HeaderName -> RequestHeadersCodec h
requestHeader = RequestHeader


data WithStatus status resTyp = WithStatus status resTyp deriving (Eq, Show, Lift)

-- NOTE: Current implementation has a problem that for example `WithStatus 404 (WithStatus 500 PlainText)` ignores 500. But I'll ignore the problem
instance IsStatusCode status => HasStatusCode (WithStatus status resTyp) where
  statusCodes = [NonDefaultStatus $ toUntypedStatusCode @status]

instance HasContentTypes resTyp => HasContentTypes (WithStatus status resTyp) where
  contentTypes = contentTypes @resTyp


instance
  ( IsStatusCode status
  , HasContentTypes resTyp
  , DecodeByMimeType (resTyp, resObj)
  ) => DecodeByMimeType (WithStatus status resTyp, resObj) where
  decodeByMimeType mediaType res = do
    rr <- decodeByMimeType @(resTyp, resObj) mediaType res
    return $ RawResponse (NonDefaultStatus (toUntypedStatusCode @status)) (rawHeaders rr) (rawBody rr)

instance
  ( IsStatusCode status
  , HasContentTypes resTyp
  , DecodeByMimeType (resTyp, resObj)
  ) => DecodeByMimeType (Response (WithStatus status resTyp) resObj) where
  decodeByMimeType mediaType (Response resObj) =
    decodeByMimeType @(WithStatus status resTyp, resObj) mediaType resObj


instance
  ( IsStatusCode status
  , HasContentTypes resTyp
  , EncodeByMimeType (resTyp, resObj)
  ) => EncodeByMimeType (WithStatus status resTyp, resObj) where
  encodeByMimeType mediaType rr = do
    resObj <- encodeByMimeType @(resTyp, resObj) mediaType rr
    rawSt <-
      case rawStatusCode rr of
          DefaultStatus       -> fail "Unexpected status code"
          NonDefaultStatus st -> return st
    _ <- maybe (fail "Unexpected status code") return $ fromStatusCode @status rawSt
    return resObj

instance
  ( IsStatusCode status
  , HasContentTypes resTyp
  , EncodeByMimeType (resTyp, resObj)
  ) => EncodeByMimeType (Response (WithStatus status resTyp) resObj) where
  encodeByMimeType mediaType rr =
    Response <$> encodeByMimeType @(WithStatus status resTyp, resObj) mediaType rr
