{-# LANGUAGE TemplateHaskell #-}
module WaiSample.Client
  ( declareClient
  , Backend
  ) where

import qualified Data.ByteString.Lazy       as BL
import           Data.Char                  (toUpper)
import qualified Data.Text                  as T
import           Data.Typeable              (TypeRep, tyConModule, tyConName,
                                             tyConPackage, typeOf, typeRepTyCon)
import           Language.Haskell.TH        (Dec, DecQ, DecsQ, TypeQ, appT,
                                             clause, conT, funD, mkName,
                                             normalB, sigD, varP)
import           Language.Haskell.TH.Syntax (ModName (ModName), Name (Name),
                                             NameFlavour (NameG),
                                             NameSpace (TcClsName),
                                             OccName (OccName),
                                             PkgName (PkgName))
import           WaiSample


declareClient :: String -> [Handler] -> DecsQ
declareClient prefix = fmap concat . mapM declareEndpointFunction
 where
  declareEndpointFunction :: Handler -> DecsQ
  declareEndpointFunction (Handler handlerName tbl _action toFromResponseBody) = do
    let funName = mkName $ makeUpName handlerName
        typeRepArg = getTypeRep tbl
        typeRepRtn = resObjType toFromResponseBody
        typeQRtn = [t| IO |] `appT` typeRepToTypeQ typeRepRtn
        typeQTail =
          if typeRepArg == typeOf ()
            then typeQRtn
            else [t| (->) |] `appT` typeRepToTypeQ typeRepArg `appT` typeQRtn
    sig <- sigD funName $  [t| (->) Backend |] `appT` typeQTail

    let bd = mkName "bd"
    def <- funD
      funName
      [clause [varP bd] (normalB [e| return $ T.pack "" |]) []]
    return [sig, def]

  makeUpName :: String -> String
  makeUpName handlerName =
    if null prefix
      then handlerName
      else prefix ++ toUpperFirst handlerName

  toUpperFirst :: String -> String
  toUpperFirst (first : left) = toUpper first : left
  toUpperFirst _              = error "toUpperFirst: Empty handler name!"


-- TODO: Perhaps we should fix the case where the TypeRep object has arguments (e.g. Maybe Int).
typeRepToTypeQ :: TypeRep -> TypeQ
typeRepToTypeQ rep = conT $ Name occName nameFlavour
 where
  occName = OccName $ tyConName tyCon
  nameFlavour =
    NameG
      TcClsName
      (PkgName $ tyConPackage tyCon)
      (ModName $ tyConModule tyCon)
  tyCon = typeRepTyCon rep


type Backend = String -> IO BL.ByteString
