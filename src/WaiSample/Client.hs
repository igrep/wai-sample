{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
module WaiSample.Client
  ( declareClient
  , Backend
  ) where

import           Control.Monad.State.Strict (State, evalState, get, put)
import qualified Data.ByteString.Lazy       as BL
import           Data.Char                  (toLower, toUpper)
import           Data.Proxy                 (Proxy (Proxy))
import qualified Data.Text                  as T
import           Data.Typeable              (TypeRep, tyConModule, tyConName,
                                             tyConPackage, typeOf, typeRep,
                                             typeRepTyCon)
import           Language.Haskell.TH        (DecsQ, ExpQ, Q, TypeQ, appT,
                                             clause, conT, funD, mkName,
                                             newName, normalB, sigD, stringE,
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
  declareEndpointFunction (Handler handlerName tblT action) = do
    let funName = mkName $ makeUpName handlerName
        typeRepArg = getTypeRep tblT
        tbl = withoutTypeRep tblT
        typeRepRtn = getResponseObjectType action
        typeQRtn = [t| IO |] `appT` typeRepToTypeQ typeRepRtn
        typeQTail =
          if typeRepArg == typeOf ()
            then typeQRtn
            else typeRepToTypeQ typeRepArg `funcT` typeQRtn
    sig <- sigD funName $  [t| Backend |] `funcT` typeQFromRoutingTable typeQTail tbl

    let bd = mkName "bd"
        moreArgs = argumentNamesFromRoutingTable tbl
        allArgs = varP bd : map (varP =<<) moreArgs
        p = pathBuilderFromRoutingTable moreArgs tbl
        implE = [e|
            do
              resBody <- $(varE bd) $(p)
              return $ fromResponseBody resBody
          |]
    def <- funD
      funName
      [clause allArgs (normalB implE) []]
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
typeQFromRoutingTable :: TypeQ -> RoutingTable a -> TypeQ
typeQFromRoutingTable typeQTail = foldr funcT typeQTail  . reverse . go []
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


argumentNamesFromRoutingTable :: RoutingTable a -> [Q Name]
argumentNamesFromRoutingTable = reverse . go []
 where
  go :: [Q Name] -> RoutingTable b -> [Q Name]
  go qns AnyPiece             = newName "any" : qns
  go qns (Piece _p)           = qns
  go qns (FmapPath _f tbl)    = go qns tbl
  go qns (PurePath _x)        = qns
  go qns (ApPath tblF tblA)   =
    let qns' = go qns tblF
     in go qns' tblA
  go qns (ParsedPath proxy)   = typeRepToNameQ (typeRep proxy) : qns


pathBuilderFromRoutingTable :: [Q Name] -> RoutingTable a -> ExpQ
pathBuilderFromRoutingTable qns = (`evalState` qns) . go
 where
  go :: RoutingTable b -> State [Q Name] ExpQ
  go AnyPiece = do
    arg0 <- popArgs
    return [e| $(varE =<< arg0) |]
  go (Piece p) =
    return [e| $(stringE $ T.unpack p) |]
  go (FmapPath _f tbl) =
    go tbl
  go (PurePath _x) =
    return [e| "" |]
  go (ApPath tblF tblA) = do
    eq0 <- go tblF
    eq1 <- go tblA
    return [e| $(eq0) ++ "/" ++ $(eq1) |]
  go (ParsedPath _proxy) = do
    arg0 <- popArgs
    return [e| T.unpack $ toUrlPiece $(varE =<< arg0) |]

  popArgs :: State [Q Name] (Q Name)
  popArgs = do
    args <- get
    case args of
      [] -> error "pathBuilderFromRoutingTable: Assertion failure: No more argument names!"
      (arg0 : argsLeft) -> do
        put argsLeft
        return arg0

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


typeRepToNameQ :: TypeRep -> Q Name
typeRepToNameQ rep = newName namePrefix
 where
  namePrefix = toLowerFirst $ tyConName tyCon
  tyCon = typeRepTyCon rep

  toLowerFirst :: String -> String
  toLowerFirst (first : left) = toLower first : left
  toLowerFirst _              = error "toLowerFirst: Empty handler name!"


-- | Generate a type 'a -> b'
funcT :: TypeQ -> TypeQ -> TypeQ
funcT a b = [t| (->) |] `appT` a `appT` b

infixr 1 `funcT`


type Backend = String -> IO BL.ByteString
