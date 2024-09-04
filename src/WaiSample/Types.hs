{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module WaiSample.Types
  ( Route (..)
  , Handler (..)
  , Responder
  , SimpleResponder

  , EndpointOptions (..)
  , options
  , RequestInfo (..)
  , WithStatus (..)

  , module WaiSample.Types.ContentTypes

  , module WaiSample.Types.Request.QueryParams
  , module WaiSample.Types.Request.Headers

  , module WaiSample.Types.Status
  , module WaiSample.Types.Response.Headers
  , module WaiSample.Types.Response
  , module WaiSample.Types.Response.Sum
  ) where

import           Data.Proxy                          (Proxy (Proxy))
import qualified Data.Text                           as T
import           Data.Typeable                       (Typeable)
import           Data.Void                           (Void)
import           Language.Haskell.TH.Syntax          (Lift)
import           Network.HTTP.Types.Method           (Method)
import           Web.HttpApiData                     (FromHttpApiData,
                                                      ToHttpApiData)

import           WaiSample.Types.ContentTypes
import           WaiSample.Types.Request.Headers
import           WaiSample.Types.Request.QueryParams
import           WaiSample.Types.Response
import           WaiSample.Types.Response.Headers
import           WaiSample.Types.Response.Sum
import           WaiSample.Types.Status


data Route p where
  LiteralPath :: T.Text -> Route T.Text

  -- TODO: Rename into FmapRoute, PureRoute and ApRoute.
  -- | '<$>'
  FmapPath :: (pa -> pb) -> Route pa -> Route pb
  PurePath :: p -> Route p
  -- | '<*>'
  ApPath :: Route (pa -> pb) -> Route pa -> Route pb
  ParsedPath :: (ToHttpApiData p, FromHttpApiData p, Typeable p) => Route p

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
      , Typeable q
      , Typeable h
      , ToRequestHeaders h
      , FromRequestHeaders h
      , ToQueryParams q
      , FromQueryParams q
      , ShowRequestHeadersType h
      )
    =>
    { handlerResponseSpec :: Proxy resSpec
    , handlerName :: String
    , handlerMethod :: Method
    , handlerRoute :: Route p
    , handlerOptions :: EndpointOptions q h
    , handlerResponder :: Responder p q h (ResponseObject resSpec)
    } -> Handler


type Responder p q h resObj = p -> RequestInfo q h -> IO resObj

type SimpleResponder p resObj = p -> IO resObj


data EndpointOptions q h = EndpointOptions
  { queryParamsType    :: Proxy q
  , requestHeadersType :: Proxy h
  }

options :: EndpointOptions Void Void
options = EndpointOptions
  { queryParamsType    = Proxy
  , requestHeadersType = Proxy
  }

data RequestInfo q h = RequestInfo
  { queryParamsValue    :: q
  , requestHeadersValue :: h
  }


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
