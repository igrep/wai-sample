{-# LANGUAGE DeriveLift #-}

module WaiSample.Types.Status where

import           Data.Proxy                 (Proxy (Proxy))
import           Language.Haskell.TH.Syntax (Lift)
import qualified Network.HTTP.Types.Status  as HTS


data DefaultStatus = DefaultStatus deriving (Show, Eq, Lift)

class IsStatusCode status where
  toStatusCode :: status -> HTS.Status
  fromStatusCode :: HTS.Status -> Maybe status

data Status200 = Status200 deriving (Show, Eq, Lift)

instance IsStatusCode Status200 where
  toStatusCode _ = HTS.status200
  fromStatusCode st = if st == HTS.status200 then Just Status200 else Nothing

data Status400 = Status400 deriving (Show, Eq, Lift)

instance IsStatusCode Status400 where
  toStatusCode _ = HTS.status400
  fromStatusCode st = if st == HTS.status400 then Just Status400 else Nothing

data Status500 = Status500 deriving (Show, Eq, Lift)

instance IsStatusCode Status500 where
  toStatusCode _ = HTS.status500
  fromStatusCode st = if st == HTS.status500 then Just Status500 else Nothing

data Status503 = Status503 deriving (Show, Eq, Lift)

instance IsStatusCode Status503 where
  toStatusCode _ = HTS.status503
  fromStatusCode st = if st == HTS.status503 then Just Status503 else Nothing

class HasStatusCode resTyp where
  statusCodes :: Proxy resTyp -> [HTS.Status]
  statusCodes _ = []
