{-# LANGUAGE TemplateHaskell #-}
module WaiSample.Client
  ( declareClient
  , Backend
  ) where

import qualified Data.ByteString.Lazy as BL
import           Data.Char            (toUpper)
import qualified Data.Text            as T
import           Data.Typeable        (TypeRep)
import           Language.Haskell.TH  (Dec, DecQ, DecsQ, TypeQ, appT, clause,
                                       funD, mkName, normalB, sigD)
import           WaiSample


declareClient :: String -> [Handler] -> DecsQ
declareClient prefix = fmap concat . mapM declareEndpointFunction
 where
  declareEndpointFunction :: Handler -> DecsQ
  declareEndpointFunction (Handler handlerName tbl _action toFromResponseBody) = do
    let funName = mkName $ makeUpName handlerName
        typeQUrl = typeRepToTypeQ $ getTypeRep tbl
    -- TODO: 戻り値のTypeQ, 引数のRepが()の場合は引数に追加しない
    sig <- sigD funName $ [t| (->) |] `appT` [t| Backend |] `appT` typeQUrl
    def <- funD
      funName
      [clause [[p| bd |]] (normalB [e| return $ T.pack "" |]) []]
    return [sig, def]

  makeUpName :: String -> String
  makeUpName handlerName =
    if null prefix
      then prefix ++ toUpperFirst handlerName
      else handlerName

  toUpperFirst :: String -> String
  toUpperFirst (first : left) = toUpper first : left
  toUpperFirst _              = error "toUpperFirst: Empty handler name!"


typeRepToTypeQ :: TypeRep -> TypeQ
typeRepToTypeQ _ =
  error "typeRepToTypeQ is not defined yet!"


type Backend = String -> IO BL.ByteString
