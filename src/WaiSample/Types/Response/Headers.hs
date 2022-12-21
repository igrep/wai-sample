{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module WaiSample.Types.Headers where

import           Data.Kind    (Type)
import           GHC.TypeLits (Symbol)


-- TODO: HeaderとWithHeadersについて、Showのインスタンスを作り、withHeadersが意図通りの挙動になるか試す
newtype Header (name :: Symbol) (hdObj :: Type) = Header hdObj

data WithHeaders (headers :: [Type]) (resObj :: Type) where
  NoHeaders :: resObj -> WithHeaders '[] resObj
  AddHeader :: header -> WithHeaders headers resObj -> WithHeaders (header ': headers) resObj

class WrapWithHeaders (headers :: [Type]) (resObj :: Type) where
  type Wrapper headers resObj
  withHeaders :: Wrapper headers resObj

instance WrapWithHeaders '[] resObj where
  type Wrapper '[] resObj = resObj -> WithHeaders '[] resObj
  withHeaders = NoHeaders

instance WrapWithHeaders headers resObj => WrapWithHeaders (Header name hdObj ': headers) resObj where
  type Wrapper (Header name hdObj ': headers) resObj =
    hdObj -> WithHeaders headers resObj -> WithHeaders (Header name hdObj ': headers) resObj

  withHeaders = AddHeader . Header
