{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module WaiSample
  ( app
  , routes
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

import           Control.Error.Util         (hush)
import           Control.Exception          (bracket_)
import           Data.Aeson                 (FromJSON, ToJSON)
import qualified Data.Aeson                 as Json
import qualified Data.Attoparsec.Text       as AT
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Functor               (void)
import           Data.Maybe                 (listToMaybe, mapMaybe)
import           Data.Proxy                 (Proxy (Proxy))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.IO               as TIO
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import           Network.HTTP.Types.Header  (hContentType)
import           Network.HTTP.Types.Status  (status200, status404)
import           Network.Wai                (Application, Request, Response,
                                             pathInfo, responseLBS)
import           Web.HttpApiData            (FromHttpApiData, ToHttpApiData,
                                             parseUrlPiece)


app :: Application
app = handles routes


routes :: [Handler]
routes =
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
printRoutes = TIO.putStrLn $ showRoutes routes


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

type MimeType = B.ByteString

class Typeable resObj => ToFromResponseBody resObj where
  toResponseBody   :: resObj -> IO BL.ByteString
  fromResponseBody :: BL.ByteString -> IO resObj
  mimeType         :: resObj -> MimeType
  -- TODO: How to express multiple mime types of a type
  -- TODO: Add Negotiation with Content-Type
  -- TODO: Add other header, status code etc.

newtype Json a = Json { unJson :: a }

instance (ToJSON resObj, FromJSON resObj, Typeable resObj) => ToFromResponseBody (Json resObj) where
  toResponseBody = return . Json.encode . unJson
  fromResponseBody = either fail (return . Json) . Json.eitherDecode'
  mimeType _ = "application/json"

instance ToFromResponseBody T.Text where
  toResponseBody = return . BL.fromStrict . TE.encodeUtf8
  fromResponseBody = return . TE.decodeUtf8 . BL.toStrict
  mimeType _ = "text/plain;charset=UTF-8"


getResponseObjectType :: (Typeable a, Typeable resObj) => (a -> IO resObj) -> Proxy resObj
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
runHandler (Handler _name tbl hdl ) req =
  act <$> runRoutingTable tbl req
 where
  act x = do
    resObj <- hdl x
    -- TODO: Detect request methods, request body
    let mime = mimeType resObj
    resBody <- toResponseBody resObj
    return $ responseLBS status200 [(hContentType, mime)] resBody


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
