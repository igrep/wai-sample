{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module WaiSample
  ( app
  , anyPiece
  , path
  , piece
  , decimalPiece
  , Handler (..)
  ) where

import           Control.Exception         (bracket_)
import           Control.Monad             (guard)
import           Data.Maybe                (listToMaybe, mapMaybe)
import           Data.Monoid               (First (First, getFirst))
import qualified Data.Text                 as T
import qualified Data.Text.Read            as TR
import           Network.HTTP.Types.Status (status200, status404)
import           Network.Wai               (Application, Request, Response,
                                            pathInfo, responseLBS)


app :: Application
app = handles
  [ (pathIs []) `then'` (\_ -> return $ responseLBS status200 [] "index")
  , (pathIs ["about", "us"]) `then'` (\_ -> return $ responseLBS status200 [] "About IIJ")
  ]


pathIs :: [T.Text] -> Handler ()
pathIs ps = Handler $ \req -> guard $ pathInfo req == ps


newtype PathParser a = PathParser { runPathParser :: [T.Text] -> Maybe (a, [T.Text]) }


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
-- pathIs "foo/example/users" *> decimalPiece

-- :id of /for/example/users/:id
decimalPiece :: PathParser Int
decimalPiece = PathParser $ \inp -> do
  (p, ps) <- runPathParser anyPiece inp
  case TR.decimal p of
      Right (i, "") -> Just (i, ps)
      Right _       -> Nothing
      Left _        -> Nothing


path :: T.Text -> PathParser [T.Text]
path pathWithSlash = undefined -- PathParser $ \inp -> do
  -- TODO: Implment using instance of Applicative and Functor
 where
  -- TODO: Explain dropWhile, dropWhileEnd, dropAround
  ps = T.split (== '/') $ T.dropAround (== '/') pathWithSlash

newtype Handler a = Handler { runHandler :: Request -> Maybe a }

instance Semigroup (Handler a) where
  (Handler a) <> (Handler b) = Handler $ \req -> getFirst $ First (a req) <> First (b req)


then' :: Handler a -> (a -> IO Response) -> Handler (IO Response)
then' = undefined


handles :: [Handler (IO Response)] -> Application
handles handlers req respond' = bracket_ (putStrLn "Allocating") (putStrLn "Cleaning") $ do
  let foundResponds = listToMaybe $ mapMaybe (\handler -> runHandler handler req) handlers
  case foundResponds of
      Just respond -> respond' =<< respond
      Nothing      -> respond' handle404
 where
  handle404 = responseLBS status404 [] "404 Not found."
