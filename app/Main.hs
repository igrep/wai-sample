module Main where

import           Network.Wai.Handler.Warp (runEnv)

import           WaiSample (app)

main :: IO ()
main = runEnv 8080 app
