{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module WaiSample
  ( sampleApp
  , sampleRoutes
  , root
  , path
  , paramPiece
  , decimalPiece
  , Handler (..)
  , handler
  , RoutingTable (..)
  , ToFromResponseBody (..)
  , Json (..)
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
import           Network.HTTP.Media          (MediaType, matchAccept,
                                              renderHeader, (//), (/:))
import           Network.HTTP.Types.Header   (hContentType)
import           Network.HTTP.Types.Status   (status200, status404, status406)
import           Network.Wai                 (Application,
                                              Request (requestHeaders),
                                              Response, pathInfo, responseLBS)
import           Web.FormUrlEncoded          (FromForm, ToForm, urlDecodeAsForm)
import           Web.HttpApiData             (FromHttpApiData, ToHttpApiData,
                                              parseUrlPiece)
import           Web.Internal.FormUrlEncoded (urlEncodeAsForm)


sampleApp :: Application
sampleApp = handles sampleRoutes


sampleRoutes :: [Handler]
sampleRoutes =
  [ handler "index" root (\_ -> return ("index" :: T.Text))
  , handler "aboutUs" (path "about/us") (\_ -> return ("About IIJ" :: T.Text))
  , handler "aboutUsFinance" (path "about/us/finance") (\_ -> return ("Financial Report 2020" :: T.Text))
  , handler "aboutFinance" (path "about/finance") (\_ -> return ("Financial Report 2020 /" :: T.Text))
  , handler "aboutFinanceImpossible" (path "/about/finance/impossible") (\_ -> (fail "This should not be executed." :: IO T.Text))
  , handler "customerId"
      (path "customer/" *> decimalPiece)
      (\i -> return $ "Customer ID: " <> T.pack (show i))
  , handler "customerIdJson"
    -- /customer/:id.json
    (path "customer/" *> decimalPiece <* path ".json")
    (\i -> return . Json $ Customer
      { customerName = "Mr. " <> T.pack (show i)
      , customerId = i
      })
  , handler "customerTransaction"
    ( do
        path "customer/"
        cId <- decimalPiece
        path "/transaction/"
        transactionName <- T.replicate 2 <$> paramPiece
        pure (cId, transactionName)
      )
    (\(cId, transactionName) ->
      return $ "Customer " <> T.pack (show cId) <> " Transaction " <> transactionName
      )
  ]


data Customer = Customer
  { customerName :: T.Text
  , customerId   :: Integer
  } deriving (Generic, Show)

instance ToJSON Customer where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance FromJSON Customer


printRoutes :: IO ()
printRoutes = TIO.putStrLn $ showRoutes sampleRoutes


getRoutingTableType :: RoutingTable a -> Proxy a
getRoutingTableType _ = Proxy


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
  Handler :: (Typeable a, ToFromResponseBody resObj) => String -> RoutingTable a -> (a -> IO resObj) -> Handler

class Typeable resObj => ToFromResponseBody resObj where
  toResponseBody        :: MediaType -> resObj -> IO BL.ByteString
  fromResponseBody      :: MediaType -> BL.ByteString -> IO resObj
  contentTypeCandidates :: Proxy resObj -> NE.NonEmpty MediaType

  -- TODO: Add other header, status code etc.

newtype Json a = Json { unJson :: a }

instance (ToJSON resObj, FromJSON resObj, Typeable resObj) => ToFromResponseBody (Json resObj) where
  toResponseBody _        = return . Json.encode . unJson
  fromResponseBody _      = either fail (return . Json) . Json.eitherDecode'
  contentTypeCandidates _ = "application" // "json" :| []

newtype FormUrlEncoded a = FormUrlEncoded { unFormUrlEncode :: a }

instance (ToForm resObj, FromForm resObj, Typeable resObj) => ToFromResponseBody (FormUrlEncoded resObj) where
  toResponseBody _        = return . urlEncodeAsForm . unFormUrlEncode
  fromResponseBody _      = either (fail . T.unpack) (return . FormUrlEncoded) . urlDecodeAsForm
  contentTypeCandidates _ = "application" // "x-www-form-urlencoded" :| []

instance ToFromResponseBody T.Text where
  toResponseBody _        = return . BL.fromStrict . TE.encodeUtf8
  fromResponseBody _      = return . TE.decodeUtf8 . BL.toStrict
  contentTypeCandidates _ = "text" // "plain" /: ("charset", "UTF-8") :| []


getResponseObjectType :: (a -> IO resObj) -> Proxy resObj
getResponseObjectType _ = Proxy


handler :: forall a resObj. (Typeable a, ToFromResponseBody resObj) => String -> RoutingTable a -> (a -> IO resObj) -> Handler
handler = Handler


handles :: [Handler] -> Application
handles hdls req respond' = bracket_ (putStrLn "Allocating") (return ()) $ do
  let foundResponds = listToMaybe $ mapMaybe (`runHandler` req) hdls
  case foundResponds of
      Just respond -> respond' =<< respond
      Nothing      -> respond' handle404
 where
  handle404 = responseLBS status404 [] "404 Not found."


runHandler :: Handler -> Request -> Maybe (IO Response)
runHandler (Handler _name tbl hdl) req =
  act <$> runRoutingTable tbl req
 where
  act x = do
    let mMime = matchAccept (NE.toList (contentTypeCandidates (typeOfResponse hdl))) acceptHeader
    case mMime of
        Just mime -> do
          resObj <- hdl x -- TODO: Maybe pass MIME type here to optimize building response bodies.
          resBody <- toResponseBody mime resObj
          return $ responseLBS status200 [(hContentType, renderHeader mime)] resBody
        Nothing ->
          return $ responseLBS status406 [(hContentType, "text/plain; charset=UTF-8")] "406 Not Acceptable"

  acceptHeader = fromMaybe "*/*" . L.lookup "Accept" $ requestHeaders req

  typeOfResponse :: (a -> IO resObj) -> Proxy resObj
  typeOfResponse _ = Proxy


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
extractRoutingTable (Handler _name tbl _hdl) = void tbl
