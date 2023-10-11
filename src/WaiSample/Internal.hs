{-# LANGUAGE TemplateHaskellQuotes #-}

module WaiSample.Internal where

import qualified Data.ByteString.Char8      as B
import           Network.HTTP.Types         (Method, methodPost)
import qualified Network.HTTP.Types.Status  as HTS
-- For older GHC: import Language.Haskell.TH (Q, TExp, stringE)
-- For older GHC: import Language.Haskell.TH.Syntax (unsafeTExpCoerce)
import           Language.Haskell.TH.Syntax (Code, Quote, liftTyped)


-- For older GHC: liftHttpStatus :: HTS.Status -> Q (TExp HTS.Status)
liftHttpStatus :: Quote m => HTS.Status -> Code m HTS.Status
liftHttpStatus (HTS.Status sc sm) =
  -- For older GHC: [|| HTS.mkStatus sc $ B.pack $$(unsafeTExpCoerce . stringE $ B.unpack sm) ||]
  [|| HTS.mkStatus sc $ B.pack $$(liftTyped $ B.unpack sm) ||]


defaultStatusCodeOf :: Method -> HTS.Status
defaultStatusCodeOf method = if method == methodPost then HTS.status201 else HTS.status200
