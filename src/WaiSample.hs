{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module WaiSample
  ( app
  , Handler (..)
  ) where

import           Control.Exception         (bracket_)
import           Control.Monad             (guard)
import           Data.Maybe                (listToMaybe, mapMaybe)
import           Data.Monoid               (First (First, getFirst))
import qualified Data.Text                 as T
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

{-
  pathIs :: PathHandler a -> Handler a
  pathPiece :: T.Text -> PathHandler ()
-}


-- /for/example/users/:id
intPath :: Handler Int
intPath = undefined


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
