{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module WaiSample.Client
  ( declareClient
  , Backend
  , httpClientBackend
  ) where

import qualified Control.Monad.State.Strict as SS
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.CaseInsensitive       as CI
import           Data.Char                  (toLower, toUpper)
import           Data.Maybe                 (fromMaybe)
import           Data.Proxy                 (Proxy)
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable, tyConName, typeRep,
                                             typeRepTyCon)
import           Language.Haskell.TH        (DecsQ, Q, TypeQ, appT,
                                             clause, funD, mkName, newName,
                                             normalB, sigD, stringE, varE, varP)
import           Language.Haskell.TH.Syntax (Name, unsafeTExpCoerce, unTypeQ)
import           LiftType                   (liftTypeQ)
import           Network.HTTP.Client        (Manager, httpLbs, parseUrlThrow,
                                             responseBody, responseHeaders,
                                             responseStatus,
                                             setRequestIgnoreStatus)
import qualified Network.HTTP.Client        as HC
import           Network.HTTP.Media         (parseAccept)
import           Network.HTTP.Types.Method  (Method)
import qualified Network.URI.Encode         as URI
import           Safe                       (headNote)
import           Web.HttpApiData            (toUrlPiece)

import           WaiSample
import           WaiSample.Internal
import Language.Haskell.TH (TExp)


declareClient :: String -> [Handler] -> DecsQ
declareClient prefix = fmap concat . mapM declareEndpointFunction
 where
  declareEndpointFunction :: Handler -> DecsQ
  declareEndpointFunction (Handler (_ :: Proxy resSpec) handlerName meth tbl action) = do
    let funName = mkName $ makeUpName handlerName
        typeRtn = getResponseObjectType action
        typeQRtn = [t| IO |] `appT` typeToTypeQ typeRtn
    sig <- sigD funName $  [t| Backend |] `funcT` typeQFromRoutingTable typeQRtn tbl

    let bd = mkName "bd"
        emsg = "Default MIME type not defined for " ++ show handlerName
        defaultMimeType = show . headNote emsg $ contentTypes @(ResponseType resSpec)
    moreArgs <- argumentNamesFromRoutingTable tbl
    let allArgs = varP bd : map varP moreArgs
        p = pathBuilderFromRoutingTable moreArgs tbl
        defaultStatus = liftHttpStatus $ defaultStatusCodeOf meth
        u = unsafeTExpCoerce
        implE = unTypeQ [||
            do
              res <- $$(u $ varE bd) (B.pack $$(u . stringE $ B.unpack meth)) $ URI.encode $$(p)
              let headerName = CI.mk $ B.pack "Content-Type"
                  contentTypeFromServer = lookup headerName $ responseHeaders res
                  returnedContentType = fromMaybe (B.pack defaultMimeType) contentTypeFromServer
                  mContentType = parseAccept returnedContentType
              contentType <- maybe
                (fail $ "Invalid Content-Type returned from the server: " ++ show returnedContentType)
                return
                mContentType
              let rres = RawResponse
                    { rawBody = responseBody res
                    , rawStatusCode =
                      if responseStatus res == $$(defaultStatus)
                        then DefaultStatus
                        else NonDefaultStatus $ responseStatus res
                    }
              fromRawResponse contentType rres
          ||]
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
  go tqs (LiteralPath _p)   = tqs
  go tqs (FmapPath _f tbl)  = go tqs tbl
  go tqs (PurePath _x)      = tqs
  go tqs (ApPath tblF tblA) =
    let tqs' = go tqs tblF
     in go tqs' tblA
  go tqs (ParsedPath proxy) = typeToTypeQ proxy : tqs


argumentNamesFromRoutingTable :: RoutingTable a -> Q [Name]
argumentNamesFromRoutingTable = sequence . reverse . go []
 where
  go :: [Q Name] -> RoutingTable b -> [Q Name]
  go qns (LiteralPath _p)   = qns
  go qns (FmapPath _f tbl)  = go qns tbl
  go qns (PurePath _x)      = qns
  go qns (ApPath tblF tblA) =
    let qns' = go qns tblF
     in go qns' tblA
  go qns (ParsedPath proxy) = typeToNameQ proxy : qns


pathBuilderFromRoutingTable :: [Name] -> RoutingTable a -> Q (TExp String)
pathBuilderFromRoutingTable qns = (`SS.evalState` qns) . go
 where
  go :: RoutingTable b -> SS.State [Name] (Q (TExp String))
  go (LiteralPath p) =
    return [|| $$(u . stringE $ T.unpack p) ||]
  go (FmapPath _f tbl) =
    go tbl
  go (PurePath _x) =
    return [|| "" ||]
  go (ApPath tblF tblA) = do
    eq0 <- go tblF
    eq1 <- go tblA
    return [|| $$(eq0) ++ $$(eq1) ||]
  go (ParsedPath _proxy) = do
    arg0 <- popArgs
    return [|| T.unpack $ toUrlPiece ($$(u $ varE arg0) :: String) ||]

  u = unsafeTExpCoerce

  popArgs :: SS.State [Name] Name
  popArgs = do
    args <- SS.get
    case args of
      [] -> error "pathBuilderFromRoutingTable: Assertion failure: No more argument names!"
      (arg0 : argsLeft) -> do
        SS.put argsLeft
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


type Backend = Method -> String -> IO (HC.Response BL.ByteString)


httpClientBackend :: String -> Manager -> Backend
httpClientBackend rootUrl manager method pathPieces = do
  req <- parseUrlThrow $ B.unpack method ++ " " ++ rootUrl ++ pathPieces
  httpLbs (setRequestIgnoreStatus  req) manager
