{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module WaiSample
  ( app
  , anyPiece
  , routes
  , path
  , pathWithSlashes
  , piece
  , decimalPiece
  , Handler (..)
  , RoutingTable (..)
  , RoutingTableWithType (..)
  , ToFromResponseBody (..)
  , Json (..)
  , getTypeRep
  , getResponseObjectType
  , withoutTypeRep
  , showRoutes
  , printRoutes
  ) where

import           Control.Exception          (bracket_)
import           Control.Monad              (guard)
import           Data.Aeson                 (FromJSON, ToJSON)
import qualified Data.Aeson                 as Json
import           Data.Bifunctor             (first)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Foldable              (traverse_)
import           Data.Functor               (void)
import           Data.Maybe                 (listToMaybe, mapMaybe)
import           Data.Monoid                (First (First, getFirst))
import           Data.Proxy                 (Proxy (Proxy))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Lazy.Builder     as TLB
import qualified Data.Text.Lazy.IO          as TLIO
import qualified Data.Text.Read             as TR
import           Data.Typeable              (TypeRep, Typeable, typeOf, typeRep,
                                             typeRepArgs)
import           GHC.Generics               (Generic)
import           Network.HTTP.Types.Status  (status200, status404)
import           Network.Wai                (Application, Request, Response,
                                             pathInfo, responseLBS)

import qualified WaiSample.PathParser       as PathParser


app :: Application
app = handles routes


routes :: [Handler]
routes =
  [ handler "index" (path "/") (\_ -> return ("index" :: T.Text))
  , handler "aboutUs" (path "/about/us") (\_ -> return ("About IIJ" :: T.Text))
  , handler "aboutUsFinance" (path "/about/us/finance") (\_ -> return ("Financial Report 2020" :: T.Text))
  , handler "aboutFinance" (path "/about/finance") (\_ -> return ("Financial Report 2020 /" :: T.Text))
  , handler "aboutFinanceImpossible" (path "/about//finance") (\_ -> (fail "This should not be executed." :: IO T.Text))
  , handler "customerId"
      (path "/customer/" *> decimalPiece)
      (\i -> return $ "Customer ID: " <> T.pack (show i))
  , handler "customerIdJson"
    (path "/customer/" *> decimalPiece <* Piece "json")
    (\i -> return . Json $ Customer
      { customerName = "Mr. " <> T.pack (show i)
      , customerId = i
      })
  ]


{- TODO Generate functions:

prefixIndex :: Backend -> IO T.Text
prefixAboutUs :: Backend -> IO T.Text
prefixAboutUsFinance :: Backend -> IO T.Text
prefixFinance :: Backend -> IO T.Text
prefixFinanceImpossible :: Backend -> IO T.Text
prefixCustomerId :: Backend -> Integer -> IO T.Text
prefixCustomerIdJson :: Backend -> Integer -> IO Customer
-}


data Customer = Customer
  { customerName :: T.Text
  , customerId   :: Integer
  } deriving (Generic, Show)
-- TODO: Genericについて解説

instance ToJSON Customer where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance FromJSON Customer


printRoutes :: IO ()
printRoutes = TLIO.putStrLn . TLB.toLazyText $ showRoutes routes


data RoutingTableWithType a = RoutingTableWithType TypeRep (RoutingTable a)


getTypeRep :: RoutingTableWithType a -> TypeRep
getTypeRep (RoutingTableWithType rep _) = rep


withoutTypeRep :: RoutingTableWithType a -> RoutingTable a
withoutTypeRep (RoutingTableWithType _rep tbl) = tbl


data RoutingTable a where
  AnyPiece :: RoutingTable T.Text
  Piece :: T.Text -> RoutingTable T.Text
  FmapPath :: (a -> b) -> RoutingTable a -> RoutingTable b
  -- ^ '<$>'
  PurePath :: a -> RoutingTable a
  ApPath :: RoutingTable (a -> b) -> RoutingTable a -> RoutingTable b
  -- ^ '<*>'
  AltPath :: RoutingTable a -> RoutingTable a -> RoutingTable a
  -- ^ '<>'
  ParsedPath :: (T.Text -> Either String (a, T.Text)) -> RoutingTable a

instance Functor RoutingTable where
  fmap = FmapPath

instance Applicative RoutingTable where
  pure = PurePath
  (<*>) = ApPath

instance Semigroup (RoutingTable a) where
  (<>) = AltPath


anyPiece :: RoutingTable T.Text
anyPiece = AnyPiece


piece :: T.Text -> RoutingTable T.Text
piece = Piece


-- piece "foo" *> piece "example" *> piece "users"
-- path "foo/example/users" *> decimalPiece

-- :id of /for/example/users/:id
decimalPiece :: RoutingTable Integer
decimalPiece = ParsedPath TR.decimal


path :: T.Text -> RoutingTable ()
path pathWithSlash = traverse_ piece ps
 where
  ps = filter (/= "") $ T.split (== '/') pathWithSlash


pathWithSlashes :: T.Text -> RoutingTable ()
pathWithSlashes pathWithSlash = traverse_ piece ps
 where
  ps = T.split (== '/') pathWithSlash


runRoutingTable :: RoutingTableWithType a -> Request -> Maybe a
runRoutingTable (RoutingTableWithType _rep tbl) req = do
  -- TODO: Redirect when finding consecutive slashes.
  -- TODO: Join pathInfo, then parse as raw path string.
  (x, left) <- parseByRoutingTable tbl . filter (/= "") $ pathInfo req
  guard $ null left
  return x

data Handler where
  Handler :: (Typeable a, ToFromResponseBody resObj) => String -> RoutingTableWithType a -> (a -> IO resObj) -> Handler

class Typeable resObj => ToFromResponseBody resObj where
  toResponseBody   :: resObj -> IO BL.ByteString
  fromResponseBody :: BL.ByteString -> IO resObj
  -- TODO: Add mimetype
  -- TODO: Add Negotiation with Content-Type
  -- TODO: Add other header, status code etc.

newtype Json a = Json { unJson :: a }

instance (ToJSON resObj, FromJSON resObj, Typeable resObj) => ToFromResponseBody (Json resObj) where
  toResponseBody = return . Json.encode . unJson
  fromResponseBody = either fail (return . Json) . Json.eitherDecode'

instance ToFromResponseBody T.Text where
  toResponseBody = return . BL.fromStrict . TE.encodeUtf8
  fromResponseBody = return . TE.decodeUtf8 . BL.toStrict


getResponseObjectType :: (Typeable a, Typeable resObj) => (a -> IO resObj) -> TypeRep
getResponseObjectType = last . typeRepArgs . last . typeRepArgs . typeOf


handler :: forall a resObj. (Typeable a, ToFromResponseBody resObj) => String -> RoutingTable a -> (a -> IO resObj) -> Handler
handler name tbl = Handler name (RoutingTableWithType (typeRep (Proxy :: Proxy a)) tbl)


handles :: [Handler] -> Application
handles hdls req respond' = bracket_ (putStrLn "Allocating") (putStrLn "Cleaning") $ do
  let foundResponds = listToMaybe $ mapMaybe (\hdl -> runHandler hdl req) hdls
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
    resBody <- toResponseBody =<< hdl x
    return $ responseLBS status200 [] resBody


parseByRoutingTable :: RoutingTable a -> [T.Text] -> Maybe (a, [T.Text])
parseByRoutingTable AnyPiece     = PathParser.run PathParser.anyPiece
parseByRoutingTable (Piece p) = PathParser.run $ PathParser.piece p
parseByRoutingTable (FmapPath f tbl) = \inp -> first f <$> parseByRoutingTable tbl inp
parseByRoutingTable (PurePath x) = \inp -> (Just (x, inp))
parseByRoutingTable (ApPath tblF tblA) = \inp -> do
  (f, out) <- parseByRoutingTable tblF inp
  first f <$> parseByRoutingTable tblA out
parseByRoutingTable (AltPath tblA tblB) = \inp ->
  getFirst $ First (parseByRoutingTable tblA inp) <> First (parseByRoutingTable tblB inp)
parseByRoutingTable (ParsedPath parser) = \inp ->
  case inp of
    p : ps ->
      case parser p of
          Right (x, "") -> Just (x, ps)
          Right _       -> Nothing
          Left _        -> Nothing
    [] -> Nothing


-- TODO: Delete extra slashes and newlines
showRoutes :: [Handler] -> TLB.Builder
showRoutes = ("/" <>) . foldMap ((<> "\n/") . showRoutes' . extractRoutingTable)

showRoutes' :: RoutingTable a -> TLB.Builder
showRoutes' AnyPiece             = "*"
showRoutes' (Piece p)            = TLB.fromText p
showRoutes' (FmapPath _f tbl)    = showRoutes' tbl
showRoutes' (PurePath _x)        = ""
showRoutes' (ApPath tblF tblA)   = showRoutes' tblF <> "/" <> showRoutes' tblA
showRoutes' (AltPath tblA tblB)  = showRoutes' tblA <> "\n/" <> showRoutes' tblB
showRoutes' (ParsedPath _parser) = ":param" -- TODO: Name the parameter

extractRoutingTable :: Handler -> RoutingTable ()
extractRoutingTable (Handler _name (RoutingTableWithType _rep tbl) _hdl) = void tbl
