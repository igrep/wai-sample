module Main where

import           Network.Wai.Handler.Warp (runEnv)

import           WaiSample.Server.Sample  (sampleApp)

main :: IO ()
main = runEnv 8080 sampleApp
