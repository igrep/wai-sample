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
{-# LANGUAGE UndecidableInstances  #-}

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

instance BuildHeaders accumHeaders headers resObj => BuildHeaders (accumHeader ': accumHeaders) headers resObj where
  type BuildHeadersArg (accumHeader ': accumHeaders) headers resObj = accumHeader
  type BuildHeadersResult (accumHeader ': accumHeaders) headers resObj = BuildHeadersResult accumHeaders headers resObj
  buildHeaders header = AddHeader header
  --                                     ^ ここに、 buildHeader @accumHeaders headers resObj が返す WithHeaders を渡す
  --                                     ^ そのためには、 BuildHeadersResult が WithHeaders を返すことを保証しないといけない
  --                                     ^ だが実際には、 BuildHeadersResult は WithHeaders ではなく（最終的に WithHeaders を返す）関数かも知れないし、 WithHeaders かも知れない
  --                                     ^ なので何らかの形で、組み立て途中の WithHeaders 型の値を渡せないといけないが、普通の関数合成ではそんなことできません！

type family Accumulate (accumHeaders :: [Type]) (headers :: [Type]) :: [Type] where
  Accumulate accumHeaders '[]                 = accumHeaders
  Accumulate accumHeaders (header ': headers) = Accumulate (header ': accumHeaders) headers

data Exp = Exp {
    args       :: [String]
  , resultType :: [String]
} deriving Show

bAccum :: [String] -> [String] -> Int -> Exp
bAccum accum [] i = Exp { args = bExp accum i, resultType = accum }
 where
  bExp []       i = [show i]
  bExp (x : xs) i = x : bExp xs i
bAccum accum (x : xs) i = bAccum (x : accum) xs i

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
