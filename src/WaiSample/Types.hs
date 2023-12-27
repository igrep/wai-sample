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
  , ToRequestHeaders (..)
  , FromRequestHeaders (..)
  , RequestHeaderError (..)
  , requestHeader
  , WithStatus (..)
  , module WaiSample.Types.ContentTypes
  , module WaiSample.Types.Response.Headers
  , module WaiSample.Types.Response
  , module WaiSample.Types.Response.Sum
  , module WaiSample.Types.Status
  ) where

import           Data.Functor.ProductIsomorphic   (ProductIsoApplicative,
                                                   ProductIsoFunctor, pureP,
                                                   (|$|), (|*|))
import qualified Data.List.NonEmpty               as NE
import           Data.Proxy                       (Proxy (Proxy))
import qualified Data.Text                        as T
import           Data.Typeable                    (Typeable)
import           Data.Void                        (Void)
import           Language.Haskell.TH.Syntax       (Lift)
import           Network.HTTP.Types.Method        (Method)
import           Web.HttpApiData                  (FromHttpApiData,
                                                   ToHttpApiData)

import           Network.HTTP.Types               (HeaderName, RequestHeaders)
import           WaiSample.Types.ContentTypes
import           WaiSample.Types.Response
import           WaiSample.Types.Response.Headers
import           WaiSample.Types.Response.Sum
import           WaiSample.Types.Status


data Route a where
  LiteralPath :: T.Text -> Route T.Text

  -- TODO: Rename into FmapRoute, PureRoute and ApRoute.
  -- | '|$|'
  FmapPath :: (a -> b) -> Route a -> Route b
  PurePath :: a -> Route a
  -- | '|*|'
  ApPath :: Route (a -> b) -> Route a -> Route b
  ParsedPath :: (ToHttpApiData a, FromHttpApiData a, Typeable a) => Route a

instance ProductIsoFunctor Route where
  (|$|) = FmapPath

instance ProductIsoApplicative Route where
  pureP = PurePath
  (|*|) = ApPath

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
      , ToRequestHeaders h
      , FromRequestHeaders h
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

{-
  data Hoge = Hoge
    { a :: Int --> X-FOOBAR-HOGE-A
    , b :: String --> X-FOOBAR-BAZ-B
    }
  -}

data RequestHeadersCodec h where
  RequestHeader :: (ToHttpApiData h, FromHttpApiData h, Typeable h) => HeaderName -> RequestHeadersCodec h

class ToRequestHeaders h where
  toRequestHeaders :: h -> RequestHeaders

class FromRequestHeaders h where
  fromRequestHeaders :: RequestHeaders -> Either RequestHeaderError h


data RequestHeaderError =
    NoHeaderError (NE.NonEmpty HeaderName)
  | EmptyRequestHeaderError
  | UnprocessableValueError HeaderName
  deriving (Eq, Show)


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
