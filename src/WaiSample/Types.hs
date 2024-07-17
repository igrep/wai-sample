{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

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

  , RequestHeaderCodec (..)
  , HasRequestHeaderCodec (..)
  , WithRequestHeaderCodec (..)

  , ShowRequestHeadersType (..)

  , decodeHeader
  , orHeader
  , WithStatus (..)

  , module WaiSample.Types.ContentTypes
  , module WaiSample.Types.Response.Headers
  , module WaiSample.Types.Response
  , module WaiSample.Types.Response.Sum
  , module WaiSample.Types.Status

  , GToRequestHeaders (..)
  , GFromRequestHeaders (..)
  ) where

import qualified Data.Aeson                       as A
import qualified Data.Attoparsec.ByteString       as ABS
import qualified Data.ByteString.Char8            as BS
import qualified Data.CaseInsensitive             as CI
import           Data.Kind                        (Type)
import qualified Data.List.NonEmpty               as NE
import           Data.Proxy                       (Proxy (Proxy))
import           Data.String                      (IsString)
import qualified Data.Text                        as T
import           Data.Typeable                    (Typeable, typeRep)
import           Data.Void                        (Void)
import           Language.Haskell.TH.Syntax       (Lift)
import           Network.HTTP.Types.Method        (Method)
import           Web.HttpApiData                  (FromHttpApiData,
                                                   ToHttpApiData (toHeader))

import           GHC.Base                         (Symbol)
import           GHC.Generics                     (Generic, K1 (K1), M1 (M1),
                                                   Rep, U1 (U1), from, to,
                                                   (:*:) ((:*:)),
                                                   (:+:) (L1, R1))
import           GHC.TypeLits                     (KnownSymbol, symbolVal)
import           Network.HTTP.Types               (HeaderName, RequestHeaders)
import           WaiSample.Types.ContentTypes
import           WaiSample.Types.Response
import           WaiSample.Types.Response.Headers
import           WaiSample.Types.Response.Sum
import           WaiSample.Types.Status
import           Web.Internal.HttpApiData         (parseHeader)


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


-- GHC.Generics を使って、各値コンストラクターとその中の各フィールドを「どのようにrequest headerと相互変換するか（RequestHeaderCodec）」を対応づける型クラスを定義する
data RequestHeaderCodec (n :: Symbol) v where
  RequestHeader :: (KnownSymbol n, ToHttpApiData v, FromHttpApiData v, Typeable v) => RequestHeaderCodec n v

class HasRequestHeaderCodec (n :: Symbol) v where
  requestHeaderCodec :: RequestHeaderCodec n v

instance (KnownSymbol n, ToHttpApiData v, FromHttpApiData v, Typeable v) => HasRequestHeaderCodec n v where
  requestHeaderCodec = RequestHeader

newtype WithRequestHeaderCodec (n :: Symbol) v =
  WithRequestHeaderCodec { unWithRequestHeaderCodec :: v }
  deriving stock (Eq, Ord, Read, Show, Lift, Functor)
  deriving newtype (Num, Fractional, IsString, ToHttpApiData, FromHttpApiData, A.ToJSON, A.FromJSON)

instance (KnownSymbol n, ToHttpApiData v) => ToRequestHeaders (WithRequestHeaderCodec n v) where
  toRequestHeaders (WithRequestHeaderCodec v) = [(CI.mk . BS.pack $ symbolVal (Proxy @n), toHeader v)]

instance
  (KnownSymbol n, FromHttpApiData v)
  => FromRequestHeaders (WithRequestHeaderCodec n v) where
  fromRequestHeaders rhds =
    WithRequestHeaderCodec <$> decodeHeader (CI.mk . BS.pack $ symbolVal (Proxy @n)) rhds

instance (KnownSymbol n, Typeable v) => ShowRequestHeadersType (WithRequestHeaderCodec n v) where
  showRequestHeadersType = T.pack (symbolVal (Proxy @n)) <> ": " <> T.pack (show $ typeRep (Proxy @v))


class GToRequestHeaders f where
  gToRequestHeaders :: f a -> RequestHeaders

instance GToRequestHeaders U1 where
  gToRequestHeaders U1 = []

instance ToRequestHeaders v => GToRequestHeaders (K1 i v) where
  gToRequestHeaders (K1 v) = toRequestHeaders v

-- TODO:
-- Use metadata for the request header name
instance GToRequestHeaders f => GToRequestHeaders (M1 i c f) where
  gToRequestHeaders (M1 f) = gToRequestHeaders f

instance (GToRequestHeaders f, GToRequestHeaders g) => GToRequestHeaders (f :*: g) where
  gToRequestHeaders (f :*: g) = gToRequestHeaders f <> gToRequestHeaders g

instance (GToRequestHeaders f, GToRequestHeaders g) => GToRequestHeaders (f :+: g) where
  gToRequestHeaders (L1 f) = gToRequestHeaders f
  gToRequestHeaders (R1 g) = gToRequestHeaders g

class GFromRequestHeaders f where
  gFromRequestHeaders :: RequestHeaders -> FromRequestHeadersResult (f a)

instance GFromRequestHeaders U1 where
  gFromRequestHeaders _ = pure U1

instance FromRequestHeaders v => GFromRequestHeaders (K1 i v) where
  gFromRequestHeaders rhds = K1 <$> fromRequestHeaders rhds

instance GFromRequestHeaders f => GFromRequestHeaders (M1 i c f) where
  gFromRequestHeaders rhds = M1 <$> gFromRequestHeaders rhds

instance (GFromRequestHeaders f, GFromRequestHeaders g) => GFromRequestHeaders (f :*: g) where
  gFromRequestHeaders rhds = (:*:) <$> gFromRequestHeaders rhds <*> gFromRequestHeaders rhds

instance (GFromRequestHeaders f, GFromRequestHeaders g) => GFromRequestHeaders (f :+: g) where
  gFromRequestHeaders rhds =
    (L1 <$> gFromRequestHeaders rhds) `orHeader` (R1 <$> gFromRequestHeaders rhds)


class ToRequestHeaders h where
  toRequestHeaders :: h -> RequestHeaders
  default toRequestHeaders :: (Generic h, GToRequestHeaders (Rep h)) => h -> RequestHeaders
  toRequestHeaders = gToRequestHeaders . from

class FromRequestHeaders h where
  fromRequestHeaders :: RequestHeaders -> FromRequestHeadersResult h
  default fromRequestHeaders :: (Generic h, GFromRequestHeaders (Rep h)) => RequestHeaders -> FromRequestHeadersResult h
  fromRequestHeaders rhds = to <$> gFromRequestHeaders rhds


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
orHeader frhr1@(FromRequestHeadersResult eh1) frhr2@(FromRequestHeadersResult eh2) =
  case eh1 of
    Right v -> pure v
    Left (NoHeaderError hdns1) ->
      case eh2 of
        Right _ -> frhr2
        Left (NoHeaderError hdns2) ->
          FromRequestHeadersResult . Left $ NoHeaderError (hdns1 <> hdns2)
          --                                               ^^^^^^^^^^^^^^
          --                                             TODO: Is this correct to append?
        Left (UnprocessableValueError _hdn) -> frhr2
    Left (UnprocessableValueError _hdn) -> frhr1


decodeHeader :: FromHttpApiData h => HeaderName -> RequestHeaders -> FromRequestHeadersResult h
decodeHeader hdn rhds =
  case lookup hdn rhds of
    Nothing ->
      FromRequestHeadersResult . Left . NoHeaderError $ hdn NE.:| []
    Just v  ->
      case ABS.parseOnly (parseHeader <* ABS.endOfInput) v of
        Left _err -> FromRequestHeadersResult . Left $ UnprocessableValueError hdn
        Right r   -> pure r


instance ToRequestHeaders a => ToRequestHeaders (Maybe a) where
  toRequestHeaders = maybe [] toRequestHeaders

instance FromRequestHeaders a => FromRequestHeaders (Maybe a) where
  fromRequestHeaders hds = (Just <$> fromRequestHeaders hds) `orHeader` pure Nothing


instance ToRequestHeaders Void where
  toRequestHeaders = const []

instance FromRequestHeaders Void where
  fromRequestHeaders = const (pure undefined)


data RequestHeaderError =
    NoHeaderError (NE.NonEmpty HeaderName)
  | UnprocessableValueError HeaderName
  deriving (Eq, Show)


class ShowRequestHeadersType h where
  showRequestHeadersType :: T.Text
  -- TODO: Add default to ToRequestHeaders and FromRequestHeaders
  default showRequestHeadersType :: GShowRequestHeadersType (Rep h) => T.Text
  showRequestHeadersType = gShowRequestHeadersType @(Rep h)


instance ShowRequestHeadersType Void where
  showRequestHeadersType = "(none)"

instance ShowRequestHeadersType h => ShowRequestHeadersType (Maybe h) where
  showRequestHeadersType = "(optional " <> showRequestHeadersType @h <> ")"


class GShowRequestHeadersType (f :: Type -> Type) where
  gShowRequestHeadersType :: T.Text

instance GShowRequestHeadersType U1 where
  gShowRequestHeadersType = ""

instance ShowRequestHeadersType c => GShowRequestHeadersType (K1 i c) where
  gShowRequestHeadersType = showRequestHeadersType @c

instance GShowRequestHeadersType f => GShowRequestHeadersType (M1 i c f) where
  gShowRequestHeadersType = gShowRequestHeadersType @f

instance (GShowRequestHeadersType f, GShowRequestHeadersType g) => GShowRequestHeadersType (f :*: g) where
  -- NOTE: & is the strongest operator here, so it doesn't need parentheses
  gShowRequestHeadersType = gShowRequestHeadersType @f <> " & " <> gShowRequestHeadersType @g

instance (GShowRequestHeadersType f, GShowRequestHeadersType g) => GShowRequestHeadersType (f :+: g) where
  gShowRequestHeadersType = "(" <> gShowRequestHeadersType @f <> " | " <> gShowRequestHeadersType @g <> ")"


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
