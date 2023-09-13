{-# LANGUAGE AllowAmbiguousTypes #-}
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
import           Data.Proxy                 (Proxy (Proxy))
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable, tyConName, typeRep,
                                             typeRepTyCon)
import           Language.Haskell.TH        (Code (examineCode), DecsQ, ExpQ, Q,
                                             TypeQ, appT, appTypeE, clause,
                                             funD, mkName, newName, normalB,
                                             sigD, stringE, varE, varP)
import           Language.Haskell.TH.Syntax (Name, unTypeQ)
import           LiftType                   (liftTypeQ)
import           Network.HTTP.Client        (Manager, httpLbs, parseUrlThrow,
                                             responseBody, responseHeaders,
                                             responseStatus,
                                             setRequestIgnoreStatus)
import qualified Network.HTTP.Client        as HC
import           Network.HTTP.Media         (parseAccept)
import           Network.HTTP.Types         (Method, RequestHeaders)
import qualified Network.URI.Encode         as URI
import           Safe                       (headNote)
import           WaiSample
import           WaiSample.Internal
import           Web.HttpApiData            (toHeader, toUrlPiece)


declareClient :: String -> [Handler] -> DecsQ
declareClient prefix = fmap concat . mapM declareEndpointFunction
 where
  declareEndpointFunction :: Handler -> DecsQ
  declareEndpointFunction
    ( Handler
        (_ :: Proxy resSpec)
        handlerName
        meth
        tbl
        (_opts :: EndpointOptions h)
        (_responder :: Responder a h resObj)
    ) = do
    let funName = mkName $ makeUpName handlerName
        typeQResSpec = liftTypeQ @resSpec
        typeQRtn = [t| IO |] `appT` liftTypeQ @resObj
        typeQTail = liftTypeQ @h `funcT` typeQRtn
    sig <- sigD funName $  [t| Backend |] `funcT` typeQFromRoutingTable typeQTail tbl

    let bd = mkName "bd"
        emsg = "Default MIME type not defined for " ++ show handlerName
        defaultMimeType = show . headNote emsg $ contentTypes @(ResponseType resSpec)
        hdArg = mkName "reqHds"

    -- TODO: Don't generate anything if the hdParser is empty
    pathArgs <- argumentNamesFromRoutingTable tbl
    let allArgs = varP bd : map varP (pathArgs ++ [hdArg])
        p = pathBuilderFromRoutingTable pathArgs tbl
        hd = headerBuilderFromHeaderCodec @h hdArg
        defaultStatus = liftHttpStatus $ defaultStatusCodeOf meth
        implE = [|
            do
              let uri = URI.encode $(p)
                  rawReqHds = $(hd)
              res <- $(varE bd) $(liftByteString meth) uri rawReqHds
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
                      if responseStatus res == $(unTypeQ $ examineCode defaultStatus)
                        then DefaultStatus
                        else NonDefaultStatus $ responseStatus res
                    , rawHeaders = responseHeaders res
                    }
              $([| fromRawResponse |] `appTypeE` typeQResSpec) contentType rres
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
typeQFromRoutingTable :: TypeQ -> Route a -> TypeQ
typeQFromRoutingTable typeQTail = foldr funcT typeQTail  . reverse . go []
 where
  go :: forall b. [TypeQ] -> Route b -> [TypeQ]
  go tqs (LiteralPath _p)   = tqs
  go tqs (FmapPath _f tbl)  = go tqs tbl
  go tqs (PurePath _x)      = tqs
  go tqs (ApPath tblF tblA) =
    let tqs' = go tqs tblF
     in go tqs' tblA
  go tqs ParsedPath = liftTypeQ @b : tqs


argumentNamesFromRoutingTable :: Route a -> Q [Name]
argumentNamesFromRoutingTable = sequence . reverse . go []
 where
  go :: forall b. [Q Name] -> Route b -> [Q Name]
  go qns (LiteralPath _p)   = qns
  go qns (FmapPath _f tbl)  = go qns tbl
  go qns (PurePath _x)      = qns
  go qns (ApPath tblF tblA) =
    let qns' = go qns tblF
     in go qns' tblA
  go qns ParsedPath = typeToNameQ @b : qns


pathBuilderFromRoutingTable :: [Name] -> Route a -> ExpQ
pathBuilderFromRoutingTable qns = (`SS.evalState` qns) . go
 where
  go :: Route b -> SS.State [Name] ExpQ
  go (LiteralPath p) =
    return [| $(stringE $ T.unpack p) |]
  -- TODO: I'm not really sure ignoring f here is correct. Test it.
  go (FmapPath _f tbl) =
    go tbl
  go (PurePath _x) =
    return [| "" |]
  go (ApPath tblF tblA) = do
    eq0 <- go tblF
    eq1 <- go tblA
    return [| $(eq0) ++ $(eq1) |]
  go ParsedPath = do
    arg0 <- popArgs
    return [| T.unpack $ toUrlPiece $(varE arg0) |]

  popArgs :: SS.State [Name] Name
  popArgs = do
    args <- SS.get
    case args of
      [] -> error "pathBuilderFromRoutingTable: Assertion failure: No more argument names!"
      (arg0 : argsLeft) -> do
        SS.put argsLeft
        return arg0


headerBuilderFromHeaderCodec :: forall h. HasRequestHeadersCodec h => Name -> ExpQ
headerBuilderFromHeaderCodec qn = f [| $(varE qn) |] (requestHeadersCodec @h)
 where
  -- b :: RequestHeaders
  f :: ExpQ -> RequestHeadersCodec i -> ExpQ
  f expq (RequestHeader hn)         = [| [($(liftCIBS hn), toHeader $(expq))] |]
  f _expq EmptyRequestHeader        = [| [] |]
  -- TODO: I'm not really sure ignoring f here is correct. Test it.
  f expq (FmapRequestHeader _f vrh) = f expq vrh
  f _expq (PureRequestHeader _v)    = [| [] |]
  f expq (ApRequestHeader frh vrh)  = [| $(f expq frh) ++ $(f expq vrh) |]
  f expq (AltRequestHeader arh brh) = [| $(f expq arh) ++ $(f expq brh) |]


typeToNameQ :: forall t. Typeable t => Q Name
typeToNameQ = newName namePrefix
 where
  namePrefix = toLowerFirst $ tyConName tyCon
  tyCon = typeRepTyCon $ typeRep (Proxy :: Proxy t)

  toLowerFirst :: String -> String
  toLowerFirst (first : left) = toLower first : left
  toLowerFirst _              = error "toLowerFirst: Empty handler name!"


-- | Generate a type 'a -> b'
funcT :: TypeQ -> TypeQ -> TypeQ
funcT a b = [t| (->) |] `appT` a `appT` b

infixr 1 `funcT`


liftByteString :: B.ByteString -> ExpQ
liftByteString bs = [| B.pack $(stringE $ B.unpack bs) |]


liftCIBS :: CI.CI B.ByteString -> ExpQ
liftCIBS cibs = [| CI.mk $(liftByteString $ CI.foldedCase cibs) |]


type Backend = Method -> String -> RequestHeaders -> IO (HC.Response BL.ByteString)


httpClientBackend :: String -> Manager -> Backend
httpClientBackend rootUrl manager method pathPieces rawReqHds = do
  req0 <- parseUrlThrow $ B.unpack method ++ " " ++ rootUrl ++ pathPieces
  let req = req0 { HC.requestHeaders = rawReqHds }
  httpLbs (setRequestIgnoreStatus req) manager
