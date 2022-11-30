{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module WaiSample.Types.Status where

import           Language.Haskell.TH.Syntax (Lift, liftTyped)
import qualified Network.HTTP.Types.Status  as HTS

import           WaiSample.Internal (liftHttpStatus)

data StatusCodeInfo = DefaultStatus | NonDefaultStatus HTS.Status deriving (Show, Eq)

instance Lift StatusCodeInfo where
  liftTyped DefaultStatus = [|| DefaultStatus ||]
  liftTyped (NonDefaultStatus st) =
    [|| NonDefaultStatus $$(liftHttpStatus st) ||]

class IsStatusCode status where
  toUntypedStatusCode :: HTS.Status
  fromStatusCode :: HTS.Status -> Maybe status

data Status200 = Status200 deriving (Show, Eq, Lift)

instance IsStatusCode Status200 where
  toUntypedStatusCode = HTS.status200
  fromStatusCode st = if st == HTS.status200 then Just Status200 else Nothing

data Status400 = Status400 deriving (Show, Eq, Lift)

instance IsStatusCode Status400 where
  toUntypedStatusCode = HTS.status400
  fromStatusCode st = if st == HTS.status400 then Just Status400 else Nothing

data Status500 = Status500 deriving (Show, Eq, Lift)

instance IsStatusCode Status500 where
  toUntypedStatusCode = HTS.status500
  fromStatusCode st = if st == HTS.status500 then Just Status500 else Nothing

data Status503 = Status503 deriving (Show, Eq, Lift)

instance IsStatusCode Status503 where
  toUntypedStatusCode = HTS.status503
  fromStatusCode st = if st == HTS.status503 then Just Status503 else Nothing

class HasStatusCode resTyp where
  statusCodes :: [StatusCodeInfo]
  statusCodes = [DefaultStatus]

instance {-# OVERLAPPABLE #-} HasStatusCode resTyp
