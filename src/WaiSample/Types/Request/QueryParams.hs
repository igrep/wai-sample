{-# LANGUAGE AllowAmbiguousTypes        #-}
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

module WaiSample.Types.Request.QueryParams where

import qualified Data.Aeson                 as A
import qualified Data.Attoparsec.Text       as AT
import qualified Data.ByteString.UTF8       as BS
import           Data.Kind                  (Type)
import qualified Data.List.NonEmpty         as NE
import           Data.Proxy                 (Proxy (Proxy))
import           Data.String                (IsString)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Data.Typeable              (Typeable, typeRep)
import           Data.Void                  (Void)
import           GHC.Base                   (Symbol)
import           GHC.Generics               (Generic, K1 (K1), M1 (M1), Rep,
                                             U1 (U1), from, to, (:*:) ((:*:)),
                                             (:+:) (L1, R1))
import           GHC.TypeLits               (KnownSymbol, symbolVal)
import           Language.Haskell.TH.Syntax (Lift)
import           Network.HTTP.Types.URI     (SimpleQuery)
import           Web.HttpApiData            (FromHttpApiData,
                                             ToHttpApiData (toQueryParam))
import           Web.Internal.HttpApiData   (parseQueryParam)


data QueryParamCodec (n :: Symbol) v where
  QueryParam :: (KnownSymbol n, ToHttpApiData v, FromHttpApiData v, Typeable v) => QueryParamCodec n v

class HasQueryParamCodec (n :: Symbol) v where
  queryParamCodec :: QueryParamCodec n v

instance (KnownSymbol n, ToHttpApiData v, FromHttpApiData v, Typeable v) => HasQueryParamCodec n v where
  queryParamCodec = QueryParam

newtype WithQueryParamCodec (n :: Symbol) v =
  WithQueryParamCodec { unWithQueryParamCodec :: v }
  deriving stock (Eq, Ord, Read, Show, Lift, Functor)
  deriving newtype (Num, Fractional, IsString, ToHttpApiData, FromHttpApiData, A.ToJSON, A.FromJSON)

instance (KnownSymbol n, ToHttpApiData v) => ToQueryParams (WithQueryParamCodec n v) where
  toQueryParams (WithQueryParamCodec v) = [(BS.fromString $ symbolVal (Proxy @n), encodeUtf8 $ toQueryParam v)]

instance
  (KnownSymbol n, FromHttpApiData v)
  => FromQueryParams (WithQueryParamCodec n v) where
  fromQueryParams rhds =
    WithQueryParamCodec <$> decodeQuery (BS.fromString $ symbolVal (Proxy @n)) rhds

instance (KnownSymbol n, Typeable v) => ShowQueryParamsType (WithQueryParamCodec n v) where
  showQueryParamsType = T.pack (symbolVal (Proxy @n)) <> "=" <> T.pack (show $ typeRep (Proxy @v))


class GToQueryParams f where
  gToQueryParams :: f a -> SimpleQuery

instance GToQueryParams U1 where
  gToQueryParams U1 = []

instance ToQueryParams v => GToQueryParams (K1 i v) where
  gToQueryParams (K1 v) = toQueryParams v

-- TODO:
-- Use metadata for the query item name
instance GToQueryParams f => GToQueryParams (M1 i c f) where
  gToQueryParams (M1 f) = gToQueryParams f

instance (GToQueryParams f, GToQueryParams g) => GToQueryParams (f :*: g) where
  gToQueryParams (f :*: g) = gToQueryParams f <> gToQueryParams g

instance (GToQueryParams f, GToQueryParams g) => GToQueryParams (f :+: g) where
  gToQueryParams (L1 f) = gToQueryParams f
  gToQueryParams (R1 g) = gToQueryParams g

class GFromQueryParams f where
  gFromQueryParams :: SimpleQuery -> FromQueryParamsResult (f a)

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
    (L1 <$> gFromQueryParams rhds) `orQuery` (R1 <$> gFromQueryParams rhds)


class ToQueryParams q where
  toQueryParams :: q -> SimpleQuery
  default toQueryParams :: (Generic q, GToQueryParams (Rep q)) => q -> SimpleQuery
  toQueryParams = gToQueryParams . from

class FromQueryParams q where
  fromQueryParams :: SimpleQuery -> FromQueryParamsResult q
  default fromQueryParams :: (Generic q, GFromQueryParams (Rep q)) => SimpleQuery -> FromQueryParamsResult q
  fromQueryParams rhds = to <$> gFromQueryParams rhds


newtype FromQueryParamsResult q =
  FromQueryParamsResult { unFromQueryParamsResult :: Either QueryParamError q }
  deriving stock (Eq, Show)
  deriving newtype (Functor, Applicative)


-- | Choose one from a couple of 'FromQueryParamsResult's.
--   * If the first argument is 'Right', it returns the first argument.
--   * If the first argument is 'NoQueryItemError', it returns the second argument.
--   * If the first argument is 'UnprocessableValueError', where the query value is invalid,
--     it immediately returns as an error instead of the second argument.
orQuery :: FromQueryParamsResult q -> FromQueryParamsResult q -> FromQueryParamsResult q
orQuery fqpr1@(FromQueryParamsResult eq1) fqpr2@(FromQueryParamsResult eq2) =
  case eq1 of
    Right v -> pure v
    Left (NoQueryItemError qns1) ->
      case eq2 of
        Right _ -> fqpr2
        Left (NoQueryItemError qns2) ->
          FromQueryParamsResult . Left $ NoQueryItemError (qns1 <> qns2)
          --                                               ^^^^^^^^^^^^^^
          --                                             TODO: Is this correct to append?
        Left (UnprocessableQueryValueError _qn) -> fqpr2
    Left (UnprocessableQueryValueError _qn) -> fqpr1


decodeQuery :: FromHttpApiData q => QueryItemName -> SimpleQuery -> FromQueryParamsResult q
decodeQuery qn qs =
  case lookup qn qs of
    Nothing ->
      FromQueryParamsResult . Left . NoQueryItemError $ qn NE.:| []
    Just v  ->
      case AT.parseOnly (parseQueryParam <* AT.endOfInput) $ decodeUtf8 v of
        Left _err -> FromQueryParamsResult . Left $ UnprocessableQueryValueError qn
        Right r   -> pure r


instance ToQueryParams a => ToQueryParams (Maybe a) where
  toQueryParams = maybe [] toQueryParams

instance FromQueryParams a => FromQueryParams (Maybe a) where
  fromQueryParams qs = (Just <$> fromQueryParams qs) `orQuery` pure Nothing


instance ToQueryParams Void where
  toQueryParams = const []

instance FromQueryParams Void where
  fromQueryParams = const (pure undefined)


data QueryParamError =
    NoQueryItemError (NE.NonEmpty QueryItemName)
  | UnprocessableQueryValueError QueryItemName
  deriving (Eq, Show)

type QueryItemName = BS.ByteString


class ShowQueryParamsType q where
  showQueryParamsType :: T.Text
  -- TODO: Add default to ToQueryParams and FromQueryParams
  default showQueryParamsType :: GShowQueryParamsType (Rep q) => T.Text
  showQueryParamsType = gShowQueryParamsType @(Rep q)


instance ShowQueryParamsType Void where
  showQueryParamsType = "(none)"

instance ShowQueryParamsType q => ShowQueryParamsType (Maybe q) where
  showQueryParamsType = "(optional " <> showQueryParamsType @q <> ")"


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
