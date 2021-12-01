{-# LANGUAGE OverloadedStrings #-}

module WaiSampleSpec
  ( spec
  ) where

import           Control.Monad.Reader      (ReaderT, mapReaderT)
import           Control.Monad.State.Lazy  (evalStateT)
import           Data.ByteString.Char8     ()
import qualified Data.ByteString.Lazy      as BSL
import           Network.HTTP.Types        (Header)
import qualified Network.Wai               as Wai
import qualified Network.Wai.Internal      as WaiI
import           Network.Wai.Test          (Session, assertBody, assertHeader,
                                            assertStatus, defaultRequest,
                                            request, setPath)
import           Network.Wai.Test.Internal (initState)
import           Test.Syd                  (Spec, around, describe, it)
import           WaiSample


spec :: Spec
spec =
  describe "sampleApp" . around (withWaiApp sampleApp) $ do
    it "/ is available" . runStateTClientState $ do
      let req = defaultRequest `setPath` "/"
      res <- request req
      assertStatus 200 res
      assertBody "index" res
      assertHeader "Content-Type" "text/plain;charset=UTF-8" res

    it "/about/us is available" . runStateTClientState $ do
      let req = defaultRequest `setPath` "/about/us"
      res <- request req
      assertStatus 200 res
      assertBody "About IIJ" res
      assertHeader "Content-Type" "text/plain;charset=UTF-8" res

    it "/about/us/finance is available" . runStateTClientState $ do
      let req = defaultRequest `setPath` "/about/us/finance"
      res <- request req
      assertStatus 200 res
      assertBody "Financial Report 2021" res
      assertHeader "Content-Type" "text/plain;charset=UTF-8" res

    it "/about/finance/impossible isn't accessible" . runStateTClientState $ do
      let req = defaultRequest `setPath` "/about/finance/impossible"
      res <- request req
      assertStatus 404 res
      assertHeader "Content-Type" "text/plain;charset=UTF-8" res

    it "/customer/:customerId returns application/json given */* as the Accept header" . runStateTClientState $ do
      let cId = "1752"
          req = defaultRequest
                  `setPath` ("/customer/" <> BSL.toStrict cId)
                  `addHeader` ("Accept", "*/*")
      res <- request req
      assertStatus 200 res
      let expectedBody =
            "{\"customerName\":\"Mr. " <> cId <> "\","
              <> "\"customerId\":"
              <> cId
              <> "}"
      assertBody expectedBody res
      assertHeader "Content-Type" "application/json" res

    it "/customer/:customerId returns 406 given an unknown Accept header" . runStateTClientState $ do
      let cId = "1752"
          req = defaultRequest
                  `setPath` ("/customer/" <> BSL.toStrict cId)
                  `addHeader` ("Accept", "image/*")
      res <- request req
      assertStatus 406 res
      assertBody "406 Not Acceptable" res
      assertHeader "Content-Type" "text/plain;charset=UTF-8" res

    it "/customer/:customerId.json returns a JSON" . runStateTClientState $ do
      let cId = "1752"
          req = defaultRequest
                  `setPath` ("/customer/" <> BSL.toStrict cId <> ".json")
                  `addHeader` ("Accept", "text/html,*/*")
      res <- request req
      assertStatus 200 res
      let expectedBody =
            "{\"customerName\":\"Mr. " <> cId <> "\","
              <> "\"customerId\":"
              <> cId
              <> "}"
      assertBody expectedBody res
      assertHeader "Content-Type" "application/json" res


addHeader :: Wai.Request -> Header -> Wai.Request
addHeader r h = r { WaiI.requestHeaders = h : WaiI.requestHeaders r }


withWaiApp :: Wai.Application -> (Wai.Application -> IO ()) -> IO ()
withWaiApp app body = body app


runStateTClientState :: Session () -> ReaderT Wai.Application IO ()
runStateTClientState = mapReaderT (`evalStateT` initState)
