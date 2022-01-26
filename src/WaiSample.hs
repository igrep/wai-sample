{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveLift                #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}

module WaiSample
  ( sampleApp
  , runSampleApp
  , sampleRoutes
  , Customer (..)

  , root
  , path
  , paramPiece
  , decimalPiece
  , Handler (..)
  , handler

  , get
  , post
  , put
  , delete
  , patch

  , Response (..)

  , RoutingTable (..)
  , HasContentTypes (..)
  , ToResponseBody (..)
  , FromResponseBody (..)
  , Json (..)
  , FormUrlEncoded (..)
  , PlainText (..)
  , ChooseResponseType (..)
  , getRoutingTableType
  , getResponseObjectType
  , showRoutes
  , printRoutes
  ) where

import           Control.Error.Util          (hush)
import           Control.Exception           (bracket_)
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.Aeson                  as Json
import qualified Data.Attoparsec.Text        as AT
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Lazy.Char8  as BL
import qualified Data.Foldable               as F
import           Data.Functor                (void)
import qualified Data.List                   as L
import           Data.List.NonEmpty          (NonEmpty ((:|)))
import qualified Data.List.NonEmpty          as NE
import           Data.Maybe                  (fromMaybe, listToMaybe, mapMaybe)
import           Data.Proxy                  (Proxy (Proxy))
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import qualified Data.Text.IO                as TIO
import           Data.Typeable               (Typeable)
import           GHC.Generics                (Generic)
import           Language.Haskell.TH.Syntax  (Lift, liftTyped)
import           Network.HTTP.Media          (MediaType, matchAccept,
                                              renderHeader, (//), (/:))
import           Network.HTTP.Types.Header   (hContentType)
import           Network.HTTP.Types.Method   (Method, methodDelete, methodGet,
                                              methodPatch, methodPost,
                                              methodPut)
import           Network.HTTP.Types.Status   (mkStatus, status200, status201,
                                              status404, status405, status406,
                                              status500)
import qualified Network.HTTP.Types.Status as HTS
import           Network.Wai                 (Application, Request (requestHeaders, requestMethod),
                                              pathInfo, responseLBS)
import qualified Network.Wai                 as Wai
import           Network.Wai.Handler.Warp    (runEnv)
import           Web.FormUrlEncoded          (FromForm, ToForm, urlDecodeAsForm)
import           Web.HttpApiData             (FromHttpApiData, ToHttpApiData,
                                              parseUrlPiece)
import           Web.Internal.FormUrlEncoded (urlEncodeAsForm)


sampleApp :: Application
sampleApp = handles sampleRoutes


runSampleApp :: IO ()
runSampleApp = runEnv 8020 sampleApp


sampleRoutes :: [Handler]
sampleRoutes =
  -- get "index" root (WithStatus status505 Json :<|> WithStatus status500 PlainText)) (\_ -> return $ body (Right "index" :: Either Error T.Text))
  [ get "index" root (WithStatus Status500 PlainText) (\_ -> return ("index" :: T.Text))
  , get "aboutUs" (path "about/us") PlainText (\_ -> return ("About IIJ" :: T.Text))
  , get "aboutUsFinance" (path "about/us/finance") PlainText (\_ -> return ("Financial Report 2021" :: T.Text))
  , get "aboutFinance" (path "about/finance") PlainText (\_ -> return ("Financial Report 2020 /" :: T.Text))
  -- TODO: Drop the initial slash?
  , get "aboutFinanceImpossible" (path "/about/finance/impossible") PlainText (\_ -> (fail "This should not be executed due to the leading slash" :: IO T.Text))
  , get "customerId"
      (path "customer/" *> decimalPiece)
      (Json :<|> FormUrlEncoded)
      (return . customerOfId)
  , get "customerIdJson"
    -- /customer/:id.json
    (path "customer/" *> decimalPiece <* path ".json")
    Json
    (return . customerOfId)
  , get "customerTransaction"
    ( do
        path "customer/"
        cId <- decimalPiece
        path "/transaction/"
        transactionName <- paramPiece
        pure (cId, transactionName)
      )
    PlainText
    (\(cId, transactionName) ->
      return $ "Customer " <> T.pack (show cId) <> " Transaction " <> transactionName
      )
  , post "createProduct"
      (path "products")
      PlainText
      (\_ -> return ("Product created" :: T.Text))
  ]
 where
  customerOfId i =
    Customer
      { customerName = "Mr. " <> T.pack (show i)
      , customerId = i
      }


data Customer = Customer
  { customerName :: T.Text
  , customerId   :: Integer
  } deriving (Eq, Generic, Show)

instance ToJSON Customer where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance FromJSON Customer

instance ToForm Customer

instance FromForm Customer

printRoutes :: IO ()
printRoutes = TIO.putStrLn $ showRoutes sampleRoutes


getRoutingTableType :: RoutingTable a -> Proxy a
getRoutingTableType _ = Proxy


-- TODO: Rename into Route?
data RoutingTable a where
  LiteralPath :: T.Text -> RoutingTable T.Text
  -- | '<$>'
  FmapPath :: (a -> b) -> RoutingTable a -> RoutingTable b
  PurePath :: a -> RoutingTable a
  -- | '<*>'
  ApPath :: RoutingTable (a -> b) -> RoutingTable a -> RoutingTable b
  ParsedPath :: (ToHttpApiData a, FromHttpApiData a, Typeable a) => Proxy a -> RoutingTable a

instance Functor RoutingTable where
  fmap = FmapPath

instance Applicative RoutingTable where
  pure = PurePath
  (<*>) = ApPath


root :: RoutingTable ()
root = pure ()


path :: T.Text -> RoutingTable T.Text
path = LiteralPath


-- :id of /for/example/users/:id
decimalPiece :: RoutingTable Integer
decimalPiece = ParsedPath Proxy


paramPiece :: forall a. (ToHttpApiData a, FromHttpApiData a, Typeable a) => RoutingTable a
paramPiece = ParsedPath (Proxy :: Proxy a)


runRoutingTable :: RoutingTable a -> Request -> Maybe a
runRoutingTable tbl =
  hush . AT.parseOnly (parserFromRoutingTable tbl <* AT.endOfInput) . T.intercalate "/" . pathInfo


data Handler where
  Handler
    ::
     ( Typeable a
     , HasStatusCode resTyp resObj
     , HasContentTypes resTyp
     , ToResponseBody resTyp resObj
     , FromResponseBody resTyp resObj
     )
    => String -> Method -> RoutingTable a -> resTyp -> (a -> IO resObj) -> Handler
    --                                                          ^^^^^^
    --                                                          Text
    --                                                      (Response Status404 Text)

data Response status resObj = Response
  { statusCode :: status
  , bodyObj    :: resObj
  } deriving (Show, Eq)
  -- TODO: Add other header etc.

data SomeResponse resTyp where
  SomeResponse :: HasStatusCode resTyp resObj => resObj -> SomeResponse resTyp

data DefaultStatus = DefaultStatus deriving (Show, Eq, Lift)

class IsStatusCode status where
  toStatusCode :: status -> HTS.Status

data Status200 = Status200 deriving (Show, Eq, Lift)

instance IsStatusCode Status200 where
  toStatusCode _ = HTS.status200

data Status400 = Status400 deriving (Show, Eq, Lift)

instance IsStatusCode Status400 where
  toStatusCode _ = HTS.status400

data Status500 = Status500 deriving (Show, Eq, Lift)

instance IsStatusCode Status500 where
  toStatusCode _ = HTS.status500

class HasStatusCode resTyp resObj where
  statusCodes :: resTyp -> [HTS.Status]
  statusCodes _ = []

class Lift resTyp => HasContentTypes resTyp where
  contentTypes :: resTyp -> NE.NonEmpty MediaType

class (HasContentTypes resTyp, Typeable resObj) => ToResponseBody resTyp resObj where
  toResponseBody :: MediaType -> resTyp -> resObj -> IO BL.ByteString

class (HasContentTypes resTyp, Typeable resObj) => FromResponseBody resTyp resObj where
  fromResponseBody :: MediaType -> resTyp -> BL.ByteString -> IO resObj

data Json = Json deriving Lift

instance HasStatusCode Json resObj

instance HasContentTypes Json where
  contentTypes _ = "application" // "json" :| []

instance (ToJSON resObj, Typeable resObj) => ToResponseBody Json resObj where
  toResponseBody _ _ = return . Json.encode

instance (FromJSON resObj, Typeable resObj) => FromResponseBody Json resObj where
  fromResponseBody _ _ = either fail return . Json.eitherDecode'

data FormUrlEncoded = FormUrlEncoded deriving Lift

instance HasStatusCode FormUrlEncoded resObj

instance HasContentTypes FormUrlEncoded where
  contentTypes _ = "application" // "x-www-form-urlencoded" :| []

instance (ToForm resObj, Typeable resObj) => ToResponseBody FormUrlEncoded resObj where
  toResponseBody _ _ = return . urlEncodeAsForm

instance (FromForm resObj, Typeable resObj) => FromResponseBody FormUrlEncoded resObj where
  fromResponseBody _ _ = either (fail . T.unpack) return . urlDecodeAsForm

data PlainText = PlainText deriving Lift

instance HasStatusCode PlainText DefaultStatus

instance HasContentTypes PlainText where
  contentTypes _ = "text" // "plain" /: ("charset", "UTF-8") :| []

instance ToResponseBody PlainText T.Text where
  toResponseBody _ _ = return . BL.fromStrict . TE.encodeUtf8

instance FromResponseBody PlainText T.Text where
  fromResponseBody _ _ = return . TE.decodeUtf8 . BL.toStrict

data ChooseResponseType a b = a :<|> b deriving Lift

--instance (HasStatusCode a resA, HasStatusCode b resB) => HasStatusCode (ChooseResponseType a b) where
  --statusCodes (a :<|> b) = statusCodes a ++ statusCodes b

instance (HasContentTypes a, HasContentTypes b) => HasContentTypes (ChooseResponseType a b) where
  contentTypes (a :<|> b) = contentTypes a <> contentTypes b

instance (ToResponseBody a resObj, ToResponseBody b resObj, Typeable resObj) => ToResponseBody (ChooseResponseType a b) resObj where
  toResponseBody mediaType (a :<|> b) resObj
    | mediaType `F.elem` contentTypes a = toResponseBody mediaType a resObj
    | mediaType `F.elem` contentTypes b = toResponseBody mediaType b resObj
    | otherwise = fail "No suitable media type"

instance (FromResponseBody a resObj, FromResponseBody b resObj) => FromResponseBody (ChooseResponseType a b) resObj where
  fromResponseBody mediaType (a :<|> b) bs
    | mediaType `F.elem` contentTypes a = fromResponseBody mediaType a bs
    | mediaType `F.elem` contentTypes b = fromResponseBody mediaType b bs
    | otherwise = fail "No suitable media type" -- Perhaps should improve this error message.

data WithStatus status resTyp = WithStatus status resTyp deriving (Eq, Show)

instance (Lift status, Lift resTyp) => Lift (WithStatus status resTyp) where
  liftTyped (WithStatus st resTyp) = [|| WithStatus $$(liftedStatus) resTyp ||]
   where
    liftedStatus = [|| mkStatus $$(liftTyped $ HTS.statusCode st) (B.pack stMsg) ||]
    stMsg = B.unpack $ HTS.statusMessage st

-- NOTE: Current implementation has a problem that for example `WithStatus 404 (WithStatus 500 PlainText)` ignores 500. But I'll ignore the problem
instance IsStatusCode status => HasStatusCode (WithStatus status resTyp) (SomeResponse resTyp) where
  statusCodes (WithStatus st _resTyp) = [toStatusCode st]

instance (Lift status, HasContentTypes resTyp) => HasContentTypes (WithStatus status resTyp) where
  contentTypes (WithStatus _st resTyp) = contentTypes resTyp

instance (Lift status, ToResponseBody resTyp resObj) => ToResponseBody (WithStatus status resTyp) resObj where
  toResponseBody mediaType (WithStatus _st resTyp) resObj = toResponseBody mediaType resTyp resObj

instance (Lift status, FromResponseBody resTyp resObj) => FromResponseBody (WithStatus status resTyp) resObj where
  fromResponseBody mediaType (WithStatus _st resTyp) bs = fromResponseBody mediaType resTyp bs


getResponseObjectType :: (a -> IO resObj) -> Proxy resObj
getResponseObjectType _ = Proxy


handler
  :: forall a resTyp resObj.
  ( Typeable a
  , HasContentTypes resTyp
  , HasStatusCode resTyp resObj
  , ToResponseBody resTyp resObj
  , FromResponseBody resTyp resObj
  )
  => String -> Method -> RoutingTable a -> resTyp -> (a -> IO resObj) -> Handler
handler = Handler


get, post, put, delete, patch
  :: forall a resTyp resObj.
  ( Typeable a
  , HasContentTypes resTyp
  , HasStatusCode resTyp resObj
  , ToResponseBody resTyp resObj
  , FromResponseBody resTyp resObj
  )
  => String -> RoutingTable a -> resTyp -> (a -> IO resObj) -> Handler
get name    = handler name methodGet
post name   = handler name methodPost
put name    = handler name methodPut
delete name = handler name methodDelete
patch name  = handler name methodPatch


handles :: [Handler] -> Application
handles hdls req respond' = bracket_ (return ()) (return ()) $ do
  let foundResponds = listToMaybe $ mapMaybe (`runHandler` req) hdls
  case foundResponds of
      Just respond -> respond' =<< respond
      Nothing      -> respond' handle404
 where
  handle404 = responseLBS status404 [(hContentType, "text/plain;charset=UTF-8")] "404 Not found."


runHandler :: Handler -> Request -> Maybe (IO Wai.Response)
runHandler (Handler _name method tbl resTyp hdl) req =
  act <$> runRoutingTable tbl req
 where
  act x =
    if method == requestMethod req
      then do
        let mMime = matchAccept (NE.toList (contentTypes resTyp)) acceptHeader
        case mMime of
            Just mime -> do
              Response mst resObj <- hdl x
              resBody <- toResponseBody mime resTyp resObj
              let stC =
                    case mst of
                        Just st -> st
                        Nothing ->
                          if method == methodPost then status201 else status200
              return $ responseLBS stC [(hContentType, renderHeader mime)] resBody
            Nothing ->
              return $ responseLBS status406 [(hContentType, "text/plain;charset=UTF-8")] "406 Not Acceptable"
      else return $ responseLBS status405 [(hContentType, "text/plain;charset=UTF-8")] "405 Method not allowed."

  acceptHeader = fromMaybe "*/*" . L.lookup "Accept" $ requestHeaders req


parserFromRoutingTable :: forall a. RoutingTable a -> AT.Parser a
parserFromRoutingTable (LiteralPath p) = AT.string p
parserFromRoutingTable (FmapPath f tbl) = f <$> parserFromRoutingTable tbl
parserFromRoutingTable (PurePath x) = pure x
parserFromRoutingTable (ApPath tblF tblA) = parserFromRoutingTable tblF <*> parserFromRoutingTable tblA
parserFromRoutingTable (ParsedPath _) = parseUrlPiece


showRoutes :: [Handler] -> T.Text
showRoutes = ("/" <>) . T.intercalate "\n/" . map (showRoutes' . extractRoutingTable)

showRoutes' :: RoutingTable a -> T.Text
showRoutes' (LiteralPath p)    = p
showRoutes' (FmapPath _f tbl)  = showRoutes' tbl
showRoutes' (PurePath _x)      = ""
showRoutes' (ApPath tblF tblA) = showRoutes' tblF <> showRoutes' tblA
showRoutes' (ParsedPath _)     = ":param" -- TODO: Name the parameter

extractRoutingTable :: Handler -> RoutingTable ()
extractRoutingTable (Handler _name _method tbl _ctype _hdl) = void tbl
