{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeApplications #-}

module WaiSample.Types.Response.Headers where

import           Data.Kind    (Type)
import           Data.Proxy   (Proxy (Proxy))
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)


newtype Header (name :: Symbol) (hdObj :: Type) = Header hdObj

data WithHeaders (headers :: [Type]) (resObj :: Type) where
  NoHeaders :: resObj -> WithHeaders '[] resObj
  AddHeader :: header -> WithHeaders headers resObj -> WithHeaders (header ': headers) resObj

class Headered (name :: Symbol) hdObj orig new
  | name hdObj orig -> new, new -> name, new -> hdObj, new -> orig where
  headered :: hdObj -> orig -> new

instance {-# OVERLAPPING #-} Headered name hdObj resObj (WithHeaders '[Header name hdObj] resObj) where
  headered hdObj resObj = AddHeader (Header @name hdObj) (NoHeaders resObj)

-- TODO: https://hackage.haskell.org/package/servant-0.19.1/docs/src/Servant.API.ResponseHeaders.html#AddHeader を参考に直す
instance Headered name hdObj (WithHeaders headers resObj) (WithHeaders (Header name hdObj ': headers) resObj) where
