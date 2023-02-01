{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module WaiSample.Types.Response.Headers where

import           Data.Kind    (Type)
import           Data.Proxy   (Proxy (Proxy))
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)


newtype Header (name :: Symbol) (hdObj :: Type) = Header hdObj

data Headered (headers :: [Type]) (resObj :: Type) where
  NoHeaders :: resObj -> Headered '[] resObj
  AddHeader :: header -> Headered headers resObj -> Headered (header ': headers) resObj


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
