{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module WaiSample.Client
  ( declareClient
  , Backend
  ) where

import           Control.Monad.State.Strict (State, evalState, get, put)
import qualified Data.ByteString.Lazy       as BL
import           Data.Char                  (toLower, toUpper)
import           Data.Proxy                 (Proxy (Proxy))
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable, tyConName, typeRep,
                                             typeRepTyCon)
import           Language.Haskell.TH        (DecsQ, ExpQ, Q, TypeQ, appT,
                                             clause, funD, mkName, newName,
                                             normalB, sigD, stringE, varE, varP)
import           Language.Haskell.TH.Syntax (Name)
import           LiftType                   (liftTypeQ)
import           WaiSample
import           Web.HttpApiData            (toUrlPiece)


declareClient :: String -> [Handler] -> DecsQ
declareClient prefix = fmap concat . mapM declareEndpointFunction
 where
  declareEndpointFunction :: Handler -> DecsQ
  declareEndpointFunction (Handler handlerName tbl action) = do
    let funName = mkName $ makeUpName handlerName
        typeRtn = getResponseObjectType action
        typeQRtn = [t| IO |] `appT` typeToTypeQ typeRtn
    sig <- sigD funName $  [t| Backend |] `funcT` typeQFromRoutingTable typeQRtn tbl

    let bd = mkName "bd"
    moreArgs <- argumentNamesFromRoutingTable tbl
    let allArgs = varP bd : map varP moreArgs
        p = pathBuilderFromRoutingTable moreArgs tbl
        implE = [e|
            do
              resBody <- $(varE bd) $(p)
              fromResponseBody resBody
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
  go tqs (Piece _p)           = tqs
  go tqs (FmapPath _f tbl)    = go tqs tbl
  go tqs (PurePath _x)        = tqs
  go tqs (ApPath tblF tblA)   =
    let tqs' = go tqs tblF
     in go tqs' tblA
  go tqs (ParsedPath proxy)   = typeToTypeQ proxy : tqs


argumentNamesFromRoutingTable :: RoutingTable a -> Q [Name]
argumentNamesFromRoutingTable = sequence . reverse . go []
 where
  go :: [Q Name] -> RoutingTable b -> [Q Name]
  go qns (Piece _p)           = qns
  go qns (FmapPath _f tbl)    = go qns tbl
  go qns (PurePath _x)        = qns
  go qns (ApPath tblF tblA)   =
    let qns' = go qns tblF
     in go qns' tblA
  go qns (ParsedPath proxy)   = typeToNameQ proxy : qns


pathBuilderFromRoutingTable :: [Name] -> RoutingTable a -> ExpQ
pathBuilderFromRoutingTable qns = (`evalState` qns) . go
 where
  go :: RoutingTable b -> State [Name] ExpQ
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
    return [e| T.unpack $ toUrlPiece $(varE arg0) |]

  popArgs :: State [Name] Name
  popArgs = do
    args <- get
    case args of
      [] -> error "pathBuilderFromRoutingTable: Assertion failure: No more argument names!"
      (arg0 : argsLeft) -> do
        put argsLeft
        return arg0


typeToTypeQ :: forall t. Typeable t => Proxy t -> TypeQ
typeToTypeQ _ = liftTypeQ @t


typeToNameQ :: forall t. Typeable t => Proxy t -> Q Name
typeToNameQ proxy = newName namePrefix
 where
  namePrefix = toLowerFirst $ tyConName tyCon
  tyCon = typeRepTyCon $ typeRep proxy

  toLowerFirst :: String -> String
  toLowerFirst (first : left) = toLower first : left
  toLowerFirst _              = error "toLowerFirst: Empty handler name!"


-- | Generate a type 'a -> b'
funcT :: TypeQ -> TypeQ -> TypeQ
funcT a b = [t| (->) |] `appT` a `appT` b

infixr 1 `funcT`


type Backend = String -> IO BL.ByteString
