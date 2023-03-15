{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module WaiSample.Types.Response.Headers where

import           Data.Aeson                 (FromJSON, ToJSON, toEncoding,
                                             toJSON)
import           Data.Aeson.Types           (parseJSON)
import qualified Data.Attoparsec.ByteString as AttoB
import           Data.Kind                  (Type)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.String                (fromString)
import           GHC.TypeLits               (KnownSymbol, Symbol, symbolVal)
import qualified Network.HTTP.Types         as HT
import           Web.HttpApiData            (FromHttpApiData, ToHttpApiData,
                                             parseHeader, toHeader)


newtype Header (name :: Symbol) (hdObj :: Type) = Header hdObj

data Headered (headers :: [Type]) (resObj :: Type) where
  NoHeaders :: resObj -> Headered '[] resObj
  AddHeader :: header -> Headered headers resObj -> Headered (header ': headers) resObj

instance (HasHeaders headers, ToJSON resObj) => ToJSON (Headered headers resObj) where
  toJSON (NoHeaders resObj) = toJSON $ unwrapHeaders @headers resObj
  toEncoding (NoHeaders resObj) = toEncoding $ unwrapHeaders @headers resObj

instance (HasHeaders headers, FromJSON resObj) => FromJSON (Headered headers resObj) where
  parseJSON = fmap wrapWithHeaders @headers . parseJSON


-- Ref. https://hackage.haskell.org/package/servant-0.19.1/docs/src/Servant.API.ResponseHeaders.html#AddHeader
class BuildHeadered (name :: Symbol) hdObj orig new
  | name hdObj orig -> new, new -> name, new -> hdObj, new -> orig where
  headered :: hdObj -> orig -> new

instance {-# OVERLAPPABLE #-}
  new ~ Headered '[Header name hdObj] resObj =>
  BuildHeadered
    name
    hdObj
    resObj
    new where
  headered hdObj resObj = AddHeader (Header @name hdObj) (NoHeaders resObj)

instance
  BuildHeadered
    name
    hdObj
    (Headered (header ': headers) resObj)
    (Headered (Header name hdObj ': header ': headers) resObj) where
  headered hdObj = AddHeader (Header @name hdObj)


class UnwrapHeadered (headers :: [Type]) (resObj :: Type) where
  unwrapHeadered :: Headered headers resObj -> ([HT.Header], resObj)

instance UnwrapHeadered '[] resObj where
  unwrapHeadered (NoHeaders resObj) = ([], resObj)

instance
  ( UnwrapHeadered headers resObj
  , KnownSymbol name
  , ToHttpApiData hdObj
  )
  => UnwrapHeadered (Header name hdObj ': headers) resObj where
  unwrapHeadered (AddHeader header rest) =
    let (headers, resObj) = unwrapHeadered rest
     in (toRawHeader header : headers, resObj)


class TryWrappingWithHeaders (headers :: [Type]) (resObj :: Type) where
  tryWrappingWithHeaders :: HT.ResponseHeaders -> resObj -> Either String (Headered headers resObj)

instance TryWrappingWithHeaders '[] resObj where
  tryWrappingWithHeaders _ = Right . NoHeaders

instance
  ( TryWrappingWithHeaders headers resObj
  , KnownSymbol name
  , FromHttpApiData hdObj
  )
  => TryWrappingWithHeaders (Header name hdObj ': headers) resObj where
  tryWrappingWithHeaders rawHds resObj = do
    rawHdVal <-
      case lookup hdName rawHds of
          Nothing  -> Left $ "No header " ++ show hdName ++ " matched"
          Just val -> Right val
    hdVal <- AttoB.parseOnly parseHeader rawHdVal
    restHds <- tryWrappingWithHeaders rawHds resObj
    return $ AddHeader (Header @name hdVal) restHds
   where
    hdName = toRawHeaderName @name


toRawHeader
  :: forall name hdObj
   . (KnownSymbol name, ToHttpApiData hdObj)
  => Header name hdObj -> HT.Header
toRawHeader (Header hdObj) = (toRawHeaderName @name, toHeader hdObj)


toRawHeaderName :: forall name. KnownSymbol name => HT.HeaderName
toRawHeaderName = fromString $ symbolVal (Proxy @name)
