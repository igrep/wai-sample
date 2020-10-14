{-# LANGUAGE OverloadedStrings #-}

module WaiSample
  ( app
  ) where

import           Control.Exception (bracket_)
import           Network.HTTP.Types.Status (status200)
import           Network.Wai (Application, responseLBS)


app :: Application
app _req respond = bracket_
  (putStrLn "Allocating")
  (putStrLn "Cleaning")
  (respond $ responseLBS status200 [] "Hello, World!")
