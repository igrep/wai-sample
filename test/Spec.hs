{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString.Char8 ()
import qualified Data.ByteString.Lazy  as BSL
import           Test.Syd              (describe, it, sydTest)
import           Test.Syd.Wai          (ResponseMatcher (..), get,
                                        shouldRespondWith, waiClientSpec)
import           Test.Syd.Wai.Matcher  (bodyEquals, matchAny)

import           WaiSample

main :: IO ()
main = sydTest . waiClientSpec sampleApp $
  describe "sampleApp" $ do
    it "/ is available" $ do
      let expected = ResponseMatcher
            { matchStatus = 200
            , matchHeaders = []
            , matchBody = bodyEquals "index"
            }
      get "/" `shouldRespondWith` expected

    it "/about/us is available" $ do
      let expected = ResponseMatcher
            { matchStatus = 200
            , matchHeaders = []
            , matchBody = bodyEquals "About IIJ"
            }
      get "/about/us" `shouldRespondWith` expected

    it "/about/us/finance is available" $ do
      let expected = ResponseMatcher
            { matchStatus = 200
            , matchHeaders = []
            , matchBody = bodyEquals "Financial Report 2021"
            }
      get "/about/us/finance" `shouldRespondWith` expected

    it "/about/finance/impossible isn't accessible" $ do
      let expected = ResponseMatcher
            { matchStatus = 404
            , matchHeaders = []
            , matchBody = matchAny
            }
      get "/about/finance/impossible" `shouldRespondWith` expected

    it "/customer/:customerId is available" $ do
      let customerId = "1752"
          expected = ResponseMatcher
            { matchStatus = 200
            , matchHeaders = []
            , matchBody = bodyEquals $ "Customer ID: " <> customerId
            }
      get ("/customer/" <> BSL.toStrict customerId) `shouldRespondWith` expected

    it "/customer/:customerId.json returns a JSON" $ do
      let customerId = "5817"
          expected = ResponseMatcher
            { matchStatus = 200
            , matchHeaders = []
            , matchBody = bodyEquals $
                "{\"customerName\":\"Mr. " <> customerId <> "\","
                  <> "\"customerId\":"
                  <> customerId
                  <> "}"
            }
      get ("/customer/" <> BSL.toStrict customerId <> ".json") `shouldRespondWith` expected
