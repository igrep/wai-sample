{-# LANGUAGE OverloadedStrings #-}

module WaiSample
  ( app
  ) where

import           Control.Exception         (bracket_)
import           Data.List                 (find)
import qualified Data.Text                 as T
import           Network.HTTP.Types.Status (status200, status404)
import           Network.Wai               (Application, Request, Response,
                                            pathInfo, responseLBS)


app :: Application
app = handles
  [ ([], \_req -> return $ responseLBS status200 [] "index")
  , (["about", "us"], \_req -> return $ responseLBS status200 [] "About IIJ")
  ]


type Handler = ([T.Text], Request -> IO Response)

-- TODO: Create a type convertible to `Request -> Maybe a` and the expected paths are machine readable
-- type Handler2 = (Request -> Maybe a, a -> IO Response)

-- type Handler3 = Request -> Maybe (IO Response)


handles :: [Handler] -> Application
handles handlers req respond = bracket_ (putStrLn "Allocating") (putStrLn "Cleaning") $ do
  let mhandler = snd <$> find (\(path, _action) -> path == pathInfo req) handlers
  case mhandler of
      Just handler -> respond =<< handler req
      Nothing      -> respond $ responseLBS status404 [] "404 Not found."
