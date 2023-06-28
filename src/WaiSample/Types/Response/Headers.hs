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

import qualified Data.Attoparsec.ByteString as AttoB
import           Data.Kind                  (Type)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.String                (fromString)
import           GHC.TypeLits               (KnownSymbol, Symbol, symbolVal)
import qualified Network.HTTP.Types         as HT
import           Web.HttpApiData            (FromHttpApiData, ToHttpApiData,
                                             parseHeader, toHeader)


newtype Header (name :: Symbol) (hdObj :: Type) = Header hdObj

instance (KnownSymbol name, Show hdObj) => Show (Header name hdObj) where
  show (Header hdObj) = "(Header @" ++ symbolVal (Proxy @name) ++ " " ++ show hdObj ++ ")"

instance Eq hdObj => Eq (Header name hdObj) where
  (Header hdObj0) == (Header hdObj1) = hdObj0 == hdObj1


data Headered (headers :: [Type]) (resObj :: Type) where
  NoHeaders :: resObj -> Headered '[] resObj
  AddHeader :: header -> Headered headers resObj -> Headered (header ': headers) resObj

instance Show resObj => Show (Headered '[] resObj) where
  show (NoHeaders resObj) = show resObj

instance (Show (Headered headers resObj), Show header) => Show (Headered (header ': headers) resObj) where
  show (AddHeader hdObj rest) = "(headered " ++ show hdObj ++ " " ++ show rest ++ ")"

instance Eq resObj => Eq (Headered '[] resObj) where
  (NoHeaders resObj0) == (NoHeaders resObj1) = resObj0 == resObj1

instance (Eq (Headered headers resObj), Eq header) => Eq (Headered (header ': headers) resObj) where
  (AddHeader hdObj0 rest0) == (AddHeader hdObj1 rest1) = hdObj0 == hdObj1 && rest0 == rest1


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
