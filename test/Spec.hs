{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Reader      (ReaderT, mapReaderT)
import           Control.Monad.State.Lazy  (evalStateT)
import           Data.ByteString.Char8     ()
import qualified Data.ByteString.Lazy      as BSL
import qualified Network.Wai               as Wai
import           Network.Wai.Test          (Session)
import           Network.Wai.Test.Internal (initState)
import           Test.Syd                  (around, describe, it, sydTest)
import           Test.Syd.Wai              (ResponseMatcher (..), get,
                                            shouldRespondWith, waiClientSpec)
import           Test.Syd.Wai.Matcher      (bodyEquals, matchAny)

import           WaiSample

-- TODO: ReaderT・StateT

main :: IO ()
main = sydTest .
  describe "sampleApp" . around (withWaiApp sampleApp) $ do
    it "/ is available" . runStateTClientState $ do
      let expected = ResponseMatcher
            { matchStatus = 200
            , matchHeaders = []
            , matchBody = bodyEquals "index"
            }
      get "/" `shouldRespondWith` expected

    it "/about/us is available" . runStateTClientState $ do
      let expected = ResponseMatcher
            { matchStatus = 200
            , matchHeaders = []
            , matchBody = bodyEquals "About IIJ"
            }
      get "/about/us" `shouldRespondWith` expected

    it "/about/us/finance is available" . runStateTClientState $ do
      let expected = ResponseMatcher
            { matchStatus = 200
            , matchHeaders = []
            , matchBody = bodyEquals "Financial Report 2021"
            }
      get "/about/us/finance" `shouldRespondWith` expected

    it "/about/finance/impossible isn't accessible" . runStateTClientState $ do
      let expected = ResponseMatcher
            { matchStatus = 404
            , matchHeaders = []
            , matchBody = matchAny
            }
      get "/about/finance/impossible" `shouldRespondWith` expected

    it "/customer/:customerId is available" . runStateTClientState $ do
      let customerId = "1752"
          expected = ResponseMatcher
            { matchStatus = 200
            , matchHeaders = []
            , matchBody = bodyEquals $ "Customer ID: " <> customerId
            }
      get ("/customer/" <> BSL.toStrict customerId) `shouldRespondWith` expected

    it "/customer/:customerId.json returns a JSON" . runStateTClientState $ do
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


withWaiApp :: Wai.Application -> (Wai.Application -> IO ()) -> IO ()
withWaiApp app body = body app


runStateTClientState :: Session () -> ReaderT Wai.Application IO ()
runStateTClientState = mapReaderT (`evalStateT` initState)
