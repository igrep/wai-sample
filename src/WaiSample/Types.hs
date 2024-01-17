{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module WaiSample.Types
  ( Route (..)
  , Handler (..)
  , Responder
  , SimpleResponder
  , EndpointOptions (..)
  , options
  , RequestInfo (..)
  , ToRequestHeaders (..)
  , FromRequestHeaders (..)
  , FromRequestHeadersResult (..)
  , RequestHeaderError (..)
  , decodeHeader
  , orHeader
  , WithStatus (..)
  , module WaiSample.Types.ContentTypes
  , module WaiSample.Types.Response.Headers
  , module WaiSample.Types.Response
  , module WaiSample.Types.Response.Sum
  , module WaiSample.Types.Status
  ) where

import qualified Data.Attoparsec.ByteString       as ABS
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
import           Web.Internal.HttpApiData         (parseHeader)


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


class ToRequestHeaders h where
  toRequestHeaders :: h -> RequestHeaders

class FromRequestHeaders h where
  fromRequestHeaders :: RequestHeaders -> FromRequestHeadersResult h


newtype FromRequestHeadersResult h =
  FromRequestHeadersResult { unFromRequestHeadersResult :: Either RequestHeaderError h }
  deriving stock (Eq, Show)
  deriving newtype (Functor, Applicative)


-- | Choose one from a couple of 'FromRequestHeadersResult's.
--   * If the first argument is 'Right', it returns the first argument.
--   * If the first argument is 'NoHeaderError', it returns the second argument.
--   * If the first argument is 'UnprocessableValueError', where the header value is invalid,
--     it immediately returns as an error instead of the second argument.
orHeader :: FromRequestHeadersResult h -> FromRequestHeadersResult h -> FromRequestHeadersResult h
orHeader (FromRequestHeadersResult frhr1) (FromRequestHeadersResult frhr2) =
  case frhr1 of
    Right v -> pure v
    Left (NoHeaderError _hdns) -> FromRequestHeadersResult frhr2
    Left (UnprocessableValueError hdn) ->
      FromRequestHeadersResult . Left $ UnprocessableValueError hdn


decodeHeader :: FromHttpApiData h => HeaderName -> RequestHeaders -> FromRequestHeadersResult h
decodeHeader hdn rhds =
  case lookup hdn rhds of
    Nothing ->
      FromRequestHeadersResult . Left . NoHeaderError $ hdn NE.:| []
    Just v  ->
      case ABS.parse (parseHeader <* ABS.endOfInput) v of
        ABS.Fail {}   -> FromRequestHeadersResult . Left $ UnprocessableValueError hdn
        ABS.Partial _ -> error "Assertion failed: fromRequestHeaders: unexpected Partial returned by ABS.parse"
        ABS.Done _ r  -> pure r


instance ToRequestHeaders Void where
  toRequestHeaders = const []

instance FromRequestHeaders Void where
  fromRequestHeaders = const (pure undefined)


data RequestHeaderError =
    NoHeaderError (NE.NonEmpty HeaderName)
  | UnprocessableValueError HeaderName
  deriving (Eq, Show)


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
