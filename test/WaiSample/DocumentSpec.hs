{-# LANGUAGE OverloadedStrings #-}

module WaiSample.DocumentSpec
  ( spec
  ) where

import           Safe             (findJust)
import           Test.Syd         (Spec, describe, it, shouldBe)

import           WaiSample
import           WaiSample.Sample


spec :: Spec
spec = describe "showHandlerSpec" $ do
  it "returns getExampleRequestHeaders' path, query parameters, response headers, and request headers information" $ do
    let getExampleRequestHeaders =
          findJust (\hdr -> handlerName hdr == "getExampleRequestHeaders") sampleRoutes
        expected = "getExampleRequestHeaders \"GET\" /exampleRequestHeaders/\n\
                   \  Request:\n\
                   \    Query Params: (none)\n\
                   \    Headers: (X-API-VERSION: Integer | X-API-REVISION: Integer) & X-API-KEY: Text\n\
                   \  Response: (Json,ExampleRequestHeaders)\n"
    showHandlerSpec getExampleRequestHeaders `shouldBe` expected

  it "returns getExampleQueryParams' path, query parameters, response headers, and request headers information" $ do
    let getExampleRequestHeaders =
          findJust (\hdr -> handlerName hdr == "getExampleQueryParams") sampleRoutes
        expected = "getExampleQueryParams \"GET\" /exampleQueryParams/\n\
                   \  Request:\n\
                   \    Query Params: (apiVersion: Integer | apiRevision: Integer) & apiKey: Text\n\
                   \    Headers: (none)\n\
                   \  Response: (Json,ExampleQueryParams)\n"
    showHandlerSpec getExampleRequestHeaders `shouldBe` expected
