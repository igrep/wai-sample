{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString.Char8 ()
import           Network.Wai.Test
import           Test.HUnit

import           WaiSample

main :: IO ()
main = runTestTTAndExit $ TestList
  [ TestLabel "sampleApp" . TestCase . (`runSession` sampleApp) $ do
      let req = defaultRequest `setPath` "/"
      res <- request req
      assertStatus 200 res
  ]
