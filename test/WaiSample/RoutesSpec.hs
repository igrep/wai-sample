{-# LANGUAGE OverloadedStrings #-}

module WaiSample.RoutesSpec
  ( spec
  ) where

import           Safe             (findJust)
import           Test.Syd         (Spec, describe, it, shouldBe)

import           WaiSample
import           WaiSample.Sample


spec :: Spec
spec = describe "showHandlerSpec" $ do
  it "returns getExampleRequestHeaders' path, response headers, and request headers information" $ do
    let getExampleRequestHeaders =
          findJust (\hdr -> handlerName hdr == "getExampleRequestHeaders") sampleRoutes
        expected = "getExampleRequestHeaders \"GET\" /exampleRequestHeaders/\n\
                   \  Request:\n\
                   \    Headers: (X-API-VERSION: Integer | X-API-REVISION: Integer) & X-API-KEY: Text\n\
                   \  Response: (Json,ExampleRequestHeaders)\n"
    showHandlerSpec getExampleRequestHeaders `shouldBe` expected
