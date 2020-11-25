{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module WaiSample
  ( app
  , anyPiece
  , path
  , piece
  , decimalPiece
  , Handler (..)
  , PathParser (..)
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


app :: Application
app = handles
  [ path "/" `then'` (\_ -> return $ responseLBS status200 [] "index")
  , path "/about/us" `then'` (\_ -> return $ responseLBS status200 [] "About IIJ")
  , path "/about/us/finance" `then'` (\_ -> return $ responseLBS status200 [] "Financial Report 2020")
  , (path "/customer/" *> decimalPiece) `then'`
      (\i -> return $ responseLBS status200 [] $ "Customer ID: " <> BL.pack (show i))
  ]


newtype PathParser a = PathParser { runPathParser :: [T.Text] -> Maybe (a, [T.Text]) }

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


-- TODO: Handle PathInfo including empty path pieces correctly.
path :: T.Text -> PathParser [T.Text]
path pathWithSlash = traverse piece ps
 where
  ps = T.split (== '/') $ T.dropAround (== '/') pathWithSlash


pathParserToHandler :: PathParser a -> Handler a
pathParserToHandler p = Handler $ \req -> do
  (x, left) <- runPathParser p $ pathInfo req
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
