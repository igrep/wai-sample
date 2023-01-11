{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module WaiSample.Types.Response.Headers where

import           Data.Kind    (Type)
import           Data.Proxy   (Proxy (Proxy))
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)


-- TODO: HeaderとWithHeadersについて、Showのインスタンスを作り、withHeadersが意図通りの挙動になるか試す
newtype Header (name :: Symbol) (hdObj :: Type) = Header hdObj

instance (KnownSymbol name, Show hdObj) => Show (Header name hdObj) where
  show (Header hdObj) = "Header @" ++ show (symbolVal (Proxy @name)) ++ show hdObj

data WithHeaders (headers :: [Type]) (resObj :: Type) where
  NoHeaders :: resObj -> WithHeaders '[] resObj
  AddHeader :: header -> WithHeaders headers resObj -> WithHeaders (header ': headers) resObj

instance Show resObj => Show (WithHeaders '[] resObj) where
  show (NoHeaders resObj) = "(" ++ show resObj ++ ")"
  -- これは要らない: show (AddHeader _ _) = error "Impossible"

instance (Show hd, Show (WithHeaders hds resObj)) => Show (WithHeaders (hd ': hds) resObj) where
  show (AddHeader hd whds) = "(" ++ show hd ++ " (" ++ show whds ++ "))"

class BuildHeaders (accumHeaders :: [Type]) (headers :: [Type]) (resObj :: Type) where
  type BuildHeadersArg (accumHeaders :: [Type]) (headers :: [Type]) (resObj :: Type)
  type BuildHeadersResult (accumHeaders :: [Type]) (headers :: [Type]) (resObj :: Type)
  buildHeaders :: BuildHeadersArg accumHeaders headers resObj -> BuildHeadersResult accumHeaders headers resObj

instance BuildHeaders accumHeaders '[] resObj where
  type BuildHeadersArg accumHeaders '[] resObj = resObj
  type BuildHeadersResult accumHeaders '[] resObj = WithHeaders accumHeaders resObj
  -- TODO: accumHeaders から先頭の要素を取り出し、引数にする
  buildHeaders = NoHeaders

class WrapWithHeaders (headers :: [Type]) (resObj :: Type) where
  type Wrapper headers resObj
  withHeaders :: Wrapper headers resObj

instance WrapWithHeaders '[] resObj where
  type Wrapper '[] resObj = resObj -> WithHeaders '[] resObj
  --                                  ^^^^^^^^^^^^^^^^^^^^^^
  -- ここの戻り値が型レベルリスト全体を組み立てる必要がある
  withHeaders = NoHeaders

instance WrapWithHeaders headers resObj => WrapWithHeaders (Header name hdObj ': headers) resObj where
  type Wrapper (Header name hdObj ': headers) resObj =
    hdObj -> WithHeaders headers resObj -> WithHeaders (Header name hdObj ': headers) resObj

  withHeaders = AddHeader . Header
