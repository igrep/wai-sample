{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module WaiSample.Types.Request.QueryParams where

import qualified Data.Aeson                 as A
import qualified Data.Attoparsec.ByteString as ABS
import qualified Data.ByteString.Char8      as BS
import qualified Data.CaseInsensitive       as CI
import           Data.Kind                  (Type)
import qualified Data.List.NonEmpty         as NE
import           Data.Proxy                 (Proxy (Proxy))
import           Data.String                (IsString)
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable, typeRep)
import           Data.Void                  (Void)
import           GHC.Base                   (Symbol)
import           GHC.Generics               (Generic, K1 (K1), M1 (M1), Rep,
                                             U1 (U1), from, to, (:*:) ((:*:)),
                                             (:+:) (L1, R1))
import           GHC.TypeLits               (KnownSymbol, symbolVal)
import           Language.Haskell.TH.Syntax (Lift)
import           Network.HTTP.Types         (HeaderName, RequestHeaders)
import           Web.HttpApiData            (FromHttpApiData,
                                             ToHttpApiData (toHeader))
import           Web.Internal.HttpApiData   (parseHeader)

-- TODO: Query Params 向けに書き換える。parseHeaderではなくparseQueryParamを使うなど


data QueryParamCodec (n :: Symbol) v where
  QueryParam :: (KnownSymbol n, ToHttpApiData v, FromHttpApiData v, Typeable v) => QueryParamCodec n v

class HasQueryParamCodec (n :: Symbol) v where
  requestHeaderCodec :: QueryParamCodec n v

instance (KnownSymbol n, ToHttpApiData v, FromHttpApiData v, Typeable v) => HasQueryParamCodec n v where
  requestHeaderCodec = QueryParam

newtype WithQueryParamCodec (n :: Symbol) v =
  WithQueryParamCodec { unWithQueryParamCodec :: v }
  deriving stock (Eq, Ord, Read, Show, Lift, Functor)
  deriving newtype (Num, Fractional, IsString, ToHttpApiData, FromHttpApiData, A.ToJSON, A.FromJSON)

instance (KnownSymbol n, ToHttpApiData v) => ToQueryParams (WithQueryParamCodec n v) where
  toQueryParams (WithQueryParamCodec v) = [(CI.mk . BS.pack $ symbolVal (Proxy @n), toHeader v)]

instance
  (KnownSymbol n, FromHttpApiData v)
  => FromQueryParams (WithQueryParamCodec n v) where
  fromQueryParams rhds =
    WithQueryParamCodec <$> decodeHeader (CI.mk . BS.pack $ symbolVal (Proxy @n)) rhds

instance (KnownSymbol n, Typeable v) => ShowQueryParamsType (WithQueryParamCodec n v) where
  showQueryParamsType = T.pack (symbolVal (Proxy @n)) <> ": " <> T.pack (show $ typeRep (Proxy @v))


class GToQueryParams f where
  gToQueryParams :: f a -> QueryParams

instance GToQueryParams U1 where
  gToQueryParams U1 = []

instance ToQueryParams v => GToQueryParams (K1 i v) where
  gToQueryParams (K1 v) = toQueryParams v

-- TODO:
-- Use metadata for the request header name
instance GToQueryParams f => GToQueryParams (M1 i c f) where
  gToQueryParams (M1 f) = gToQueryParams f

instance (GToQueryParams f, GToQueryParams g) => GToQueryParams (f :*: g) where
  gToQueryParams (f :*: g) = gToQueryParams f <> gToQueryParams g

instance (GToQueryParams f, GToQueryParams g) => GToQueryParams (f :+: g) where
  gToQueryParams (L1 f) = gToQueryParams f
  gToQueryParams (R1 g) = gToQueryParams g

class GFromQueryParams f where
  gFromQueryParams :: QueryParams -> FromQueryParamsResult (f a)

instance GFromQueryParams U1 where
  gFromQueryParams _ = pure U1

instance FromQueryParams v => GFromQueryParams (K1 i v) where
  gFromQueryParams rhds = K1 <$> fromQueryParams rhds

instance GFromQueryParams f => GFromQueryParams (M1 i c f) where
  gFromQueryParams rhds = M1 <$> gFromQueryParams rhds

instance (GFromQueryParams f, GFromQueryParams g) => GFromQueryParams (f :*: g) where
  gFromQueryParams rhds = (:*:) <$> gFromQueryParams rhds <*> gFromQueryParams rhds

instance (GFromQueryParams f, GFromQueryParams g) => GFromQueryParams (f :+: g) where
  gFromQueryParams rhds =
    (L1 <$> gFromQueryParams rhds) `orHeader` (R1 <$> gFromQueryParams rhds)


class ToQueryParams h where
  toQueryParams :: h -> QueryParams
  default toQueryParams :: (Generic h, GToQueryParams (Rep h)) => h -> QueryParams
  toQueryParams = gToQueryParams . from

class FromQueryParams h where
  fromQueryParams :: QueryParams -> FromQueryParamsResult h
  default fromQueryParams :: (Generic h, GFromQueryParams (Rep h)) => QueryParams -> FromQueryParamsResult h
  fromQueryParams rhds = to <$> gFromQueryParams rhds


newtype FromQueryParamsResult h =
  FromQueryParamsResult { unFromQueryParamsResult :: Either QueryParamError h }
  deriving stock (Eq, Show)
  deriving newtype (Functor, Applicative)


-- | Choose one from a couple of 'FromQueryParamsResult's.
--   * If the first argument is 'Right', it returns the first argument.
--   * If the first argument is 'NoHeaderError', it returns the second argument.
--   * If the first argument is 'UnprocessableValueError', where the header value is invalid,
--     it immediately returns as an error instead of the second argument.
orHeader :: FromQueryParamsResult h -> FromQueryParamsResult h -> FromQueryParamsResult h
orHeader frhr1@(FromQueryParamsResult eh1) frhr2@(FromQueryParamsResult eh2) =
  case eh1 of
    Right v -> pure v
    Left (NoHeaderError hdns1) ->
      case eh2 of
        Right _ -> frhr2
        Left (NoHeaderError hdns2) ->
          FromQueryParamsResult . Left $ NoHeaderError (hdns1 <> hdns2)
          --                                               ^^^^^^^^^^^^^^
          --                                             TODO: Is this correct to append?
        Left (UnprocessableValueError _hdn) -> frhr2
    Left (UnprocessableValueError _hdn) -> frhr1


decodeHeader :: FromHttpApiData h => HeaderName -> QueryParams -> FromQueryParamsResult h
decodeHeader hdn rhds =
  case lookup hdn rhds of
    Nothing ->
      FromQueryParamsResult . Left . NoHeaderError $ hdn NE.:| []
    Just v  ->
      case ABS.parseOnly (parseHeader <* ABS.endOfInput) v of
        Left _err -> FromQueryParamsResult . Left $ UnprocessableValueError hdn
        Right r   -> pure r


instance ToQueryParams a => ToQueryParams (Maybe a) where
  toQueryParams = maybe [] toQueryParams

instance FromQueryParams a => FromQueryParams (Maybe a) where
  fromQueryParams hds = (Just <$> fromQueryParams hds) `orHeader` pure Nothing


instance ToQueryParams Void where
  toQueryParams = const []

instance FromQueryParams Void where
  fromQueryParams = const (pure undefined)


data QueryParamError =
    NoHeaderError (NE.NonEmpty HeaderName)
  | UnprocessableValueError HeaderName
  deriving (Eq, Show)


class ShowQueryParamsType h where
  showQueryParamsType :: T.Text
  -- TODO: Add default to ToQueryParams and FromQueryParams
  default showQueryParamsType :: GShowQueryParamsType (Rep h) => T.Text
  showQueryParamsType = gShowQueryParamsType @(Rep h)


instance ShowQueryParamsType Void where
  showQueryParamsType = "(none)"

instance ShowQueryParamsType h => ShowQueryParamsType (Maybe h) where
  showQueryParamsType = "(optional " <> showQueryParamsType @h <> ")"


class GShowQueryParamsType (f :: Type -> Type) where
  gShowQueryParamsType :: T.Text

instance GShowQueryParamsType U1 where
  gShowQueryParamsType = ""

instance ShowQueryParamsType c => GShowQueryParamsType (K1 i c) where
  gShowQueryParamsType = showQueryParamsType @c

instance GShowQueryParamsType f => GShowQueryParamsType (M1 i c f) where
  gShowQueryParamsType = gShowQueryParamsType @f

instance (GShowQueryParamsType f, GShowQueryParamsType g) => GShowQueryParamsType (f :*: g) where
  -- NOTE: & is the strongest operator here, so it doesn't need parentheses
  gShowQueryParamsType = gShowQueryParamsType @f <> " & " <> gShowQueryParamsType @g

instance (GShowQueryParamsType f, GShowQueryParamsType g) => GShowQueryParamsType (f :+: g) where
  gShowQueryParamsType = "(" <> gShowQueryParamsType @f <> " | " <> gShowQueryParamsType @g <> ")"
