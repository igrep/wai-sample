{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveLift                #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

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
  , body
  , status

  , RoutingTable (..)
  , ContentType (..)
  , ToResponseBody (..)
  , FromResponseBody (..)
  , Json (..)
  , FormUrlEncoded (..)
  , PlainText (..)
  , ChooseContentType (..)
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
import           Language.Haskell.TH.Syntax  (Lift)
import           Network.HTTP.Media          (MediaType, matchAccept,
                                              renderHeader, (//), (/:))
import           Network.HTTP.Types.Header   (hContentType)
import           Network.HTTP.Types.Method   (Method, methodDelete, methodGet,
                                              methodPatch, methodPost,
                                              methodPut)
import           Network.HTTP.Types.Status   (Status, status200, status201,
                                              status404, status405, status406)
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
  [ get "index" root PlainText (\_ -> return $ body ("index" :: T.Text))
  , get "aboutUs" (path "about/us") PlainText (\_ -> return $ body ("About IIJ" :: T.Text))
  , get "aboutUsFinance" (path "about/us/finance") PlainText (\_ -> return $ body ("Financial Report 2021" :: T.Text))
  , get "aboutFinance" (path "about/finance") PlainText (\_ -> return $ body ("Financial Report 2020 /" :: T.Text))
  -- TODO: Drop the initial slash?
  , get "aboutFinanceImpossible" (path "/about/finance/impossible") PlainText (\_ -> (fail "This should not be executed due to the leading slash" :: IO (Response T.Text)))
  , get "customerId"
      (path "customer/" *> decimalPiece)
      (Json :<|> FormUrlEncoded)
      (return . body . customerOfId)
  , get "customerIdJson"
    -- /customer/:id.json
    (path "customer/" *> decimalPiece <* path ".json")
    Json
    (return . body . customerOfId)
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
      return . body $ "Customer " <> T.pack (show cId) <> " Transaction " <> transactionName
      )
  , post "createProduct"
      (path "products")
      PlainText
      (\_ -> return $ body ("Product created" :: T.Text))
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
    :: (Typeable a, ContentType ctype, ToResponseBody ctype resObj, FromResponseBody ctype resObj)
    => String -> Method -> RoutingTable a -> ctype -> (a -> IO (Response resObj)) -> Handler

data Response resObj = Response
  { statusCode :: !(Maybe Status)
  , bodyObj    :: !resObj
  } deriving (Show, Eq)
  -- TODO: Add other header etc.

class Lift ctype => ContentType ctype where
  contentTypes :: ctype -> NE.NonEmpty MediaType

class (ContentType ctype, Typeable resObj) => ToResponseBody ctype resObj where
  toResponseBody :: MediaType -> ctype -> resObj -> IO BL.ByteString

class (ContentType ctype, Typeable resObj) => FromResponseBody ctype resObj where
  fromResponseBody :: MediaType -> ctype -> BL.ByteString -> IO resObj

data Json = Json deriving Lift

instance ContentType Json where
  contentTypes _ = "application" // "json" :| []

instance (ToJSON resObj, Typeable resObj) => ToResponseBody Json resObj where
  toResponseBody _ _ = return . Json.encode

instance (FromJSON resObj, Typeable resObj) => FromResponseBody Json resObj where
  fromResponseBody _ _ = either fail return . Json.eitherDecode'

data FormUrlEncoded = FormUrlEncoded deriving Lift

instance ContentType FormUrlEncoded where
  contentTypes _ = "application" // "x-www-form-urlencoded" :| []

instance (ToForm resObj, Typeable resObj) => ToResponseBody FormUrlEncoded resObj where
  toResponseBody _ _ = return . urlEncodeAsForm

instance (FromForm resObj, Typeable resObj) => FromResponseBody FormUrlEncoded resObj where
  fromResponseBody _ _ = either (fail . T.unpack) return . urlDecodeAsForm

data PlainText = PlainText deriving Lift

instance ContentType PlainText where
  contentTypes _ = "text" // "plain" /: ("charset", "UTF-8") :| []

instance ToResponseBody PlainText T.Text where
  toResponseBody _ _ = return . BL.fromStrict . TE.encodeUtf8

instance FromResponseBody PlainText T.Text where
  fromResponseBody _ _ = return . TE.decodeUtf8 . BL.toStrict

data ChooseContentType a b = a :<|> b deriving Lift

instance (ContentType a, ContentType b) => ContentType (ChooseContentType a b) where
  contentTypes (a :<|> b) = contentTypes a <> contentTypes b

instance (ToResponseBody a resObj, ToResponseBody b resObj, Typeable resObj) => ToResponseBody (ChooseContentType a b) resObj where
  toResponseBody mediaType (a :<|> b) resObj
    | mediaType `F.elem` contentTypes a = toResponseBody mediaType a resObj
    | mediaType `F.elem` contentTypes b = toResponseBody mediaType b resObj
    | otherwise = fail "No suitable media type"

instance (FromResponseBody a resObj, FromResponseBody b resObj) => FromResponseBody (ChooseContentType a b) resObj where
  fromResponseBody mediaType (a :<|> b) bs
    | mediaType `F.elem` contentTypes a = fromResponseBody mediaType a bs
    | mediaType `F.elem` contentTypes b = fromResponseBody mediaType b bs
    | otherwise = fail "No suitable media type" -- Perhaps should improve this error message.


getResponseObjectType :: (a -> IO (Response resObj)) -> Proxy resObj
getResponseObjectType _ = Proxy


handler
  :: forall a ctype resObj. (Typeable a, ContentType ctype, ToResponseBody ctype resObj, FromResponseBody ctype resObj)
  => String -> Method -> RoutingTable a -> ctype -> (a -> IO (Response resObj)) -> Handler
handler = Handler


get, post, put, delete, patch
  :: forall a ctype resObj. (Typeable a, ContentType ctype, ToResponseBody ctype resObj, FromResponseBody ctype resObj)
  => String -> RoutingTable a -> ctype -> (a -> IO (Response resObj)) -> Handler
get name    = handler name methodGet
post name   = handler name methodPost
put name    = handler name methodPut
delete name = handler name methodDelete
patch name  = handler name methodPatch


body :: resObj -> Response resObj
body = Response Nothing


status :: Status -> Response resObj -> Response resObj
status newSt (Response _oldSt b) = Response (Just newSt) b


handles :: [Handler] -> Application
handles hdls req respond' = bracket_ (return ()) (return ()) $ do
  let foundResponds = listToMaybe $ mapMaybe (`runHandler` req) hdls
  case foundResponds of
      Just respond -> respond' =<< respond
      Nothing      -> respond' handle404
 where
  handle404 = responseLBS status404 [(hContentType, "text/plain;charset=UTF-8")] "404 Not found."


runHandler :: Handler -> Request -> Maybe (IO Wai.Response)
runHandler (Handler _name method tbl ctype hdl) req =
  act <$> runRoutingTable tbl req
 where
  act x =
    if method == requestMethod req
      then do
        let mMime = matchAccept (NE.toList (contentTypes ctype)) acceptHeader
        case mMime of
            Just mime -> do
              Response mst resObj <- hdl x
              resBody <- toResponseBody mime ctype resObj
              let statusCode =
                    case mst of
                        Just st -> st
                        Nothing ->
                          if method == methodPost then status201 else status200
              return $ responseLBS statusCode [(hContentType, renderHeader mime)] resBody
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
