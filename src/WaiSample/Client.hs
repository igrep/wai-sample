{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
module WaiSample.Client
  ( declareClient
  , pathBuilderSigFromRoutingTable
  , Backend
  ) where

import qualified Data.ByteString.Lazy       as BL
import           Data.Char                  (toUpper)
import           Data.Proxy                 (Proxy (Proxy))
import qualified Data.Text                  as T
import           Data.Typeable              (TypeRep, tyConModule, tyConName,
                                             tyConPackage, typeOf, typeRep,
                                             typeRepTyCon)
import           Language.Haskell.TH        (DecsQ, ExpQ, TypeQ, appT, clause,
                                             conT, funD, mkName, normalB, sigD,
                                             varE, varP)
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
  declareEndpointFunction (Handler handlerName tbl action) = do
    let funName = mkName $ makeUpName handlerName
        typeRepArg = getTypeRep tbl
        typeRepRtn = getResponseObjectType action
        typeQRtn = [t| IO |] `appT` typeRepToTypeQ typeRepRtn
        typeQTail =
          if typeRepArg == typeOf ()
            then typeQRtn
            else typeRepToTypeQ typeRepArg `funcT` typeQRtn
    -- TODO: Use pathBuilderSigFromRoutingTable
    sig <- sigD funName $  [t| Backend |] `funcT` typeQTail

    let bd = mkName "bd"
        p = pathBuilderFromRoutingTable $ withoutTypeRep tbl
        -- TODO: generate arguments `VarP`s
        implE = [e| \aaa ->
            do
              resBody <- $(varE bd) $ $(p) aaa
              return $ fromResponseBody resBody
          |]
    def <- funD
      funName
      [clause [varP bd] (normalB implE) []]
    return [sig, def]

  makeUpName :: String -> String
  makeUpName handlerName =
    if null prefix
      then handlerName
      else prefix ++ toUpperFirst handlerName

  toUpperFirst :: String -> String
  toUpperFirst (first : left) = toUpper first : left
  toUpperFirst _              = error "toUpperFirst: Empty handler name!"


-- e.g. Integer -> Integer -> String
pathBuilderSigFromRoutingTable :: RoutingTable a -> TypeQ
pathBuilderSigFromRoutingTable = foldr funcT [t| String |] . reverse . go []
 where
  go :: [TypeQ] -> RoutingTable b -> [TypeQ]
  go tqs AnyPiece             = typeRepToTypeQ (typeRep (Proxy :: Proxy T.Text)) : tqs
  go tqs (Piece _p)           = tqs
  go tqs (FmapPath _f tbl)    = go tqs tbl
  go tqs (PurePath _x)        = tqs
  go tqs (ApPath tblF tblA)   =
    let tqs' = go tqs tblF
     in go tqs' tblA
  go tqs (ParsedPath proxy)   = typeRepToTypeQ (typeRep proxy) : tqs


pathBuilderFromRoutingTable :: RoutingTable a -> ExpQ
pathBuilderFromRoutingTable = foldr build [e| "" |] . reverse . go []
 where
  -- TODO
  build :: ExpQ -> ExpQ -> ExpQ
  build = undefined

  -- TODO
  go :: [TypeQ] -> RoutingTable b -> [ExpQ]
  go = undefined

{-
pathBuilderFromRoutingTable :: RoutingTable a -> String
pathBuilderFromRoutingTable AnyPiece             = "*"
pathBuilderFromRoutingTable (Piece p)            = T.unpack p
pathBuilderFromRoutingTable (FmapPath _f tbl)    = pathBuilderFromRoutingTable tbl
pathBuilderFromRoutingTable (PurePath _x)        = ""
pathBuilderFromRoutingTable (ApPath tblF tblA)   = pathBuilderFromRoutingTable tblF <> "/" <> pathBuilderFromRoutingTable tblA
pathBuilderFromRoutingTable (ParsedPath _parser) = undefined -- TODO
-}


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


-- | Generate a type 'a -> b'
funcT :: TypeQ -> TypeQ -> TypeQ
funcT a b = [t| (->) |] `appT` a `appT` b

infixr 1 `funcT`


type Backend = String -> IO BL.ByteString
