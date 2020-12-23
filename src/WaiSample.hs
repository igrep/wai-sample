{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}

module WaiSample
  ( app
  , anyPiece
  , path
  , pathWithSlashes
  , piece
  , decimalPiece
  , Handler (..)
  , showRoutes
  , printRoutes
  ) where

import           Control.Exception          (bracket_)
import           Control.Monad              (guard)
import           Data.Bifunctor             (first)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe                 (listToMaybe, mapMaybe)
import           Data.Monoid                (First (First, getFirst))
import qualified Data.Text                  as T
import qualified Data.Text.Read             as TR
import           Network.HTTP.Types.Status  (status200, status404)
import           Network.Wai                (Application, Request, Response,
                                             pathInfo, responseLBS)

import qualified WaiSample.PathParser       as PathParser


app :: Application
app = handles
  [ path "/" `then'` (\_ -> return $ responseLBS status200 [] "index")
  , path "/about/us" `then'` (\_ -> return $ responseLBS status200 [] "About IIJ")
  , path "/about/us/finance" `then'` (\_ -> return $ responseLBS status200 [] "Financial Report 2020")
  , path "/about/finance" `then'` (\_ -> return $ responseLBS status200 [] "Financial Report 2020 /")
  , path "/about//finance" `then'` (\_ -> fail "This should not be executed.")
  , (path "/customer/" *> decimalPiece) `then'`
      (\i -> return $ responseLBS status200 [] $ "Customer ID: " <> BL.pack (show i))
  ]


routes :: RoutingTable [T.Text]
routes = foldr1 (<>)
  [ path "/"
  , path "/about/us"
  , path "/about/us/finance"
  , path "/about/finance"
  , path "/about//finance"
  , path "/customer/" *> decimalPiece *> pure []
  ]


printRoutes :: IO ()
printRoutes = putStrLn $ showRoutes routes


data RoutingTable a where
  AnyPiece :: RoutingTable T.Text
  Piece :: T.Text -> RoutingTable T.Text
  FmapPath :: (a -> b) -> RoutingTable a -> RoutingTable b
  -- ^ <$>
  PurePath :: a -> RoutingTable a
  ApPath :: RoutingTable (a -> b) -> (RoutingTable a) -> (RoutingTable b)
  -- ^ <*>
  AltPath :: RoutingTable a -> RoutingTable a -> RoutingTable a
  -- ^ <>
  ParsedPath :: (T.Text -> Either String (a, T.Text)) -> RoutingTable a

-- Defunctionalization
--   [T.Text] -> Maybe (a, [T.Text])
--     => Non function!
--     => 関数は中身をたどれない
--          ありとあらゆる入力を与えないと、どんなパスを想定しているのかわからない！
--            servantのようにクライアントを自動生成したり、ルーティングテーブルを作ったりするのが難しい
--          実行パスを全部探索できない
--   考えられるRoutingTableの操作すべてを列挙する代数的データ型に変換する

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
decimalPiece :: RoutingTable Int
decimalPiece = ParsedPath TR.decimal


path :: T.Text -> RoutingTable [T.Text]
path pathWithSlash = traverse piece ps
 where
  ps = filter (/= "") $ T.split (== '/') pathWithSlash


pathWithSlashes :: T.Text -> RoutingTable [T.Text]
pathWithSlashes pathWithSlash = traverse piece ps
 where
  ps = T.split (== '/') pathWithSlash


pathParserToHandler :: RoutingTable a -> Handler a
pathParserToHandler p = Handler $ \req -> do
  (x, left) <- parseByRoutingTable p . filter (/= "") $ pathInfo req
  guard $ left == []
  return x

newtype Handler a = Handler { runHandler :: Request -> Maybe a }

instance Semigroup (Handler a) where
  (Handler a) <> (Handler b) = Handler $ \req -> getFirst $ First (a req) <> First (b req)

-- Then' :: RoutingTable a -> (a -> IO Response) -> Handler (IO Response)
then' :: RoutingTable a -> (a -> IO Response) -> Handler (IO Response)
then' p act = Handler $ fmap act . runHandler (pathParserToHandler p)


handles :: [Handler (IO Response)] -> Application
handles handlers req respond' = bracket_ (putStrLn "Allocating") (putStrLn "Cleaning") $ do
  let foundResponds = listToMaybe $ mapMaybe (\handler -> runHandler handler req) handlers
  case foundResponds of
      Just respond -> respond' =<< respond
      Nothing      -> respond' handle404
 where
  handle404 = responseLBS status404 [] "404 Not found."


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


showRoutes :: RoutingTable [T.Text] -> String
showRoutes = undefined


{-
instance Functor PathParser where
  fmap f p = PathParser $ \inp ->
    first f <$> runPathParser p inp

instance Applicative PathParser where
  pure x = PathParser $ \inp -> Just (x, inp)
  pf <*> px = PathParser $ \inp -> do
    (f, out) <- runPathParser pf inp
    runPathParser (fmap f px) out


anyPiece :: PathParser T.Text
anyPiece = PathParser $ \inp ->
  case inp of
      []       -> Nothing
      (p : ps) -> Just (p, ps)


pieceSatisfying :: (T.Text -> Bool) -> PathParser T.Text
pieceSatisfying predicate = PathParser $ \inp -> do
  (p, ps) <- runPathParser anyPiece inp
  if predicate p then Just (p, ps) else Nothing


piece :: T.Text -> PathParser T.Text
piece p = pieceSatisfying (== p)


-- piece "foo" *> piece "example" *> piece "users"
-- path "foo/example/users" *> decimalPiece

-- :id of /for/example/users/:id
decimalPiece :: PathParser Int
decimalPiece = PathParser $ \inp -> do
  (p, ps) <- runPathParser anyPiece inp
  case TR.decimal p of
      Right (i, "") -> Just (i, ps)
      Right _       -> Nothing
      Left _        -> Nothing


path :: T.Text -> PathParser [T.Text]
path pathWithSlash = traverse piece ps
 where
  ps = filter (/= "") $ T.split (== '/') pathWithSlash


pathWithSlashes :: T.Text -> PathParser [T.Text]
pathWithSlashes pathWithSlash = traverse piece ps
 where
  ps = T.split (== '/') pathWithSlash


pathParserToHandler :: PathParser a -> Handler a
pathParserToHandler p = Handler $ \req -> do
  (x, left) <- runPathParser p . filter (/= "") $ pathInfo req
  guard $ left == []
  return x

newtype Handler a = Handler { runHandler :: Request -> Maybe a }

instance Semigroup (Handler a) where
  (Handler a) <> (Handler b) = Handler $ \req -> getFirst $ First (a req) <> First (b req)


then' :: PathParser a -> (a -> IO Response) -> Handler (IO Response)
then' p act = Handler $ fmap act . runHandler (pathParserToHandler p)


handles :: [Handler (IO Response)] -> Application
handles handlers req respond' = bracket_ (putStrLn "Allocating") (putStrLn "Cleaning") $ do
  let foundResponds = listToMaybe $ mapMaybe (\handler -> runHandler handler req) handlers
  case foundResponds of
      Just respond -> respond' =<< respond
      Nothing      -> respond' handle404
 where
  handle404 = responseLBS status404 [] "404 Not found."
-}
