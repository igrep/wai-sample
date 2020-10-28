{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module WaiSample
  ( app
  , Handler (..)
  ) where

import           Control.Exception         (bracket_)
import           Control.Monad             (guard)
import           Data.List                 (find)
import           Data.Maybe                (fromJust, isJust)
import qualified Data.Text                 as T
import           Network.HTTP.Types.Status (status200, status404)
import           Network.Wai               (Application, Request, Response,
                                            pathInfo, responseLBS)


app :: Application
app = handles
  [ (pathIs []) `then'` (\_ -> return $ responseLBS status200 [] "index")
  , (pathIs ["about", "us"]) `then'` (\_ -> return $ responseLBS status200 [] "About IIJ")
  ]


pathIs :: [T.Text] -> Request -> Maybe ()
pathIs ps req = guard $ pathInfo req == ps


-- TODO: Create a type convertible to `Request -> Maybe a` and the expected paths are machine readable
data Handler = forall a. Handler
  { match   :: Request -> Maybe a
  , respond :: a -> IO Response
  }

newtype Handler3 = Handler3 { runHandler3 :: Request -> Maybe (IO Response) }

instance Semigroup Handler3 where
  a <> b = undefined

then' :: (Request -> Maybe a) -> (a -> IO Response) -> Handler
then' = Handler


handles :: [Handler] -> Application
handles handlers req respond' = bracket_ (putStrLn "Allocating") (putStrLn "Cleaning") $ do
  let mhandler = find (\(Handler mat _res) -> isJust $ mat req) handlers
  case mhandler of
      Just (Handler mat res) -> respond' =<< (res . fromJust $ mat req)
      Nothing                -> respond' $ responseLBS status404 [] "404 Not found."
