{-# LANGUAGE OverloadedStrings #-}

module WaiSampleSpec
  ( spec
  ) where

import           Control.Monad.Reader      (ReaderT, mapReaderT)
import           Control.Monad.State.Lazy  (evalStateT)
import           Data.ByteString.Char8     ()
import qualified Data.ByteString.Lazy      as BSL
import           Network.HTTP.Types        (Header, methodGet, methodPost)
import qualified Network.Wai               as Wai
import qualified Network.Wai.Internal      as WaiI
import           Network.Wai.Test          (Session, assertBody, assertHeader,
                                            assertStatus, defaultRequest,
                                            request, setPath)
import           Network.Wai.Test.Internal (initState)
import           Test.Syd                  (Spec, around, describe, it)
import           WaiSample.Server          (sampleApp)


-- TODO: Sum ((resTyp, resObj) ': resSpecs) の中に、有効な (resTyp, resObj) がない場合に型エラーを起こすこと
spec :: Spec
spec =
  describe "sampleApp" . around (withWaiApp sampleApp) $ do
    it "GET / is available" . runStateTClientState $ do
      let req = defaultRequest `setPath` "/"
      res <- request req
      assertStatus 200 res
      assertBody "index" res
      assertHeader "Content-Type" "text/plain;charset=UTF-8" res

    it "GET /maintenance returns 503" . runStateTClientState $ do
      let req = defaultRequest `setPath` "/maintenance"
      res <- request req
      assertStatus 503 res
      assertBody "Sorry, we are under maintenance" res
      assertHeader "Content-Type" "text/plain;charset=UTF-8" res

    it "GET /about/us is available" . runStateTClientState $ do
      let req = defaultRequest `setPath` "/about/us"
      res <- request req
      assertStatus 200 res
      assertBody "About IIJ" res
      assertHeader "Content-Type" "text/plain;charset=UTF-8" res

    it "GET /about/us/finance is available" . runStateTClientState $ do
      let req = defaultRequest `setPath` "/about/us/finance"
      res <- request req
      assertStatus 200 res
      assertBody "Financial Report 2021" res
      assertHeader "Content-Type" "text/plain;charset=UTF-8" res

    it "GET /about/finance/impossible isn't accessible" . runStateTClientState $ do
      let req = defaultRequest `setPath` "/about/finance/impossible"
      res <- request req
      assertStatus 404 res
      assertHeader "Content-Type" "text/plain;charset=UTF-8" res

    it "GET /customer/:customerId returns application/json given */* as the Accept header" . runStateTClientState $ do
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

    -- TODO: 同じ型に対して複数のContent-Typeで返しうる場合のテスト
    -- it "GET /customer/:customerId returns application/www-form-urlencoded given application/www-form-urlencoded as the Accept header" . runStateTClientState $ do

    it "GET /customer/:customerId returns 406 given an unknown Accept header" . runStateTClientState $ do
      let cId = "1752"
          req = defaultRequest
                  `setPath` ("/customer/" <> BSL.toStrict cId)
                  `addHeader` ("Accept", "image/*")
      res <- request req
      assertStatus 406 res
      assertBody "406 Not Acceptable" res
      assertHeader "Content-Type" "text/plain;charset=UTF-8" res

    it "GET /customer/:customerId.json returns a JSON" . runStateTClientState $ do
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

    it "GET /customer/:customerId.txt returns a plain text with Customer ID" . runStateTClientState $ do
      let cId = "1752"
          req = defaultRequest
                  `setPath` ("/customer/" <> BSL.toStrict cId <> ".txt")
                  `addHeader` ("Accept", "text/html,*/*")
      res <- request req
      assertStatus 200 res
      let expectedBody = "Customer " <> cId
      assertBody expectedBody res
      assertHeader "Content-Type" "text/plain;charset=UTF-8" res

    it "GET /customer/503.txt returns an error message as a plain text" . runStateTClientState $ do
      let cId = "503"
          req = defaultRequest
                  `setPath` ("/customer/" <> BSL.toStrict cId <> ".txt")
                  `addHeader` ("Accept", "text/html,*/*")
      res <- request req
      assertStatus 503 res
      let expectedBody = "error"
      assertBody expectedBody res
      assertHeader "Content-Type" "text/plain;charset=UTF-8" res

    it "POST /products is available" . runStateTClientState $ do
      let req = defaultRequest
                  { WaiI.requestMethod = methodPost }
                  `setPath` "/products"
      res <- request req
      assertStatus 201 res
      assertBody "Product created" res
      assertHeader "Content-Type" "text/plain;charset=UTF-8" res

    it "GET /products is NOT available" . runStateTClientState $ do
      let req = defaultRequest
                  { WaiI.requestMethod = methodGet }
                  `setPath` "/products"
      res <- request req
      assertStatus 405 res
      assertBody "405 Method not allowed." res
      assertHeader "Content-Type" "text/plain;charset=UTF-8" res


addHeader :: Wai.Request -> Header -> Wai.Request
addHeader r h = r { WaiI.requestHeaders = h : WaiI.requestHeaders r }


withWaiApp :: Wai.Application -> (Wai.Application -> IO ()) -> IO ()
withWaiApp app action = action app


runStateTClientState :: Session () -> ReaderT Wai.Application IO ()
runStateTClientState = mapReaderT (`evalStateT` initState)
