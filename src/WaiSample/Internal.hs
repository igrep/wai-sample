{-# LANGUAGE TemplateHaskell #-}

module WaiSample.Internal where

import qualified Data.ByteString.Char8 as B
import           Network.HTTP.Types        (Method, methodPost)
import qualified Network.HTTP.Types.Status as HTS
import Language.Haskell.TH (Q, TExp, stringE)
import Language.Haskell.TH.Syntax (unsafeTExpCoerce)


liftHttpStatus :: HTS.Status -> Q (TExp HTS.Status)
liftHttpStatus (HTS.Status sc sm) =
  [|| HTS.mkStatus sc $ B.pack $$(unsafeTExpCoerce . stringE $ B.unpack sm) ||]


defaultStatusCodeOf :: Method -> HTS.Status
defaultStatusCodeOf method = if method == methodPost then HTS.status201 else HTS.status200
