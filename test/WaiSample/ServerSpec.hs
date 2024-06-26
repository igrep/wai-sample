{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module WaiSample.ServerSpec
  ( spec
  ) where

import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader      (ReaderT, mapReaderT)
import           Control.Monad.State.Lazy  (evalStateT)
import qualified Data.Aeson                as A
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           GHC.Stack                 (HasCallStack)
import           Network.HTTP.Types        (Header, ResponseHeaders, methodGet,
                                            methodPost)
import qualified Network.Wai               as Wai
import qualified Network.Wai.Internal      as WaiI
import           Network.Wai.Test          (SResponse (SResponse, simpleBody, simpleHeaders),
                                            Session, assertBody, assertHeader,
                                            assertStatus, defaultRequest,
                                            request, setPath)
import           Network.Wai.Test.Internal (initState)
import           Test.Syd                  (Spec, around, describe, it,
                                            shouldBe, shouldMatchList)
import           Web.FormUrlEncoded        (urlDecodeAsForm)

import           WaiSample                 (unWithRequestHeaderCodec)
import           WaiSample.Sample
import           WaiSample.Server.Sample   (sampleApp)


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
              <> ",\"customerApiVersion\":null}"
      assertBody expectedBody res
      assertHeader "Content-Type" "application/json" res

    it "GET /customer/:customerId.json returns a customer with the value of X-API-VERSION request header" . runStateTClientState $ do
      let cId = "221"
          req = defaultRequest
                  `setPath` ("/customer/" <> BSL.toStrict cId <> ".json")
                  `addHeader` ("Accept", "*/*")
                  `addHeader` ("X-API-VERSION", "3")
      res <- request req
      assertStatus 200 res
      let expectedBody =
            "{\"customerName\":\"Mr. " <> cId <> "\","
              <> "\"customerId\":"
              <> cId
              <> ",\"customerApiVersion\":3}"
      assertBody expectedBody res
      assertHeader "Content-Type" "application/json" res

    it "GET /customer/:customerId.json returns an 422 error given an invalid value of X-API-VERSION request header" . runStateTClientState $ do
      let cId = "221"
          req = defaultRequest
                  `setPath` ("/customer/" <> BSL.toStrict cId <> ".json")
                  `addHeader` ("Accept", "*/*")
                  `addHeader` ("X-API-VERSION", "invalid")
      res <- request req
      assertStatus 422 res
      let expectedBody = "422 Unprocessable Entity: request header \"X-API-VERSION\" is invalid."
      assertBody expectedBody res

    it "GET /customer/:customerId.json returns a customer with the value of X-API-REVISION request header" . runStateTClientState $ do
      let cId = "222"
          req = defaultRequest
                  `setPath` ("/customer/" <> BSL.toStrict cId <> ".json")
                  `addHeader` ("Accept", "*/*")
                  `addHeader` ("X-API-REVISION", "7")
      res <- request req
      assertStatus 200 res
      let expectedBody =
            "{\"customerName\":\"Mr. " <> cId <> "\","
              <> "\"customerId\":"
              <> cId
              <> ",\"customerApiVersion\":7}"
      assertBody expectedBody res
      assertHeader "Content-Type" "application/json" res

    it "GET /customer/:customerId.json returns an 422 error given an invalid value of X-API-REVISION request header" . runStateTClientState $ do
      let cId = "223"
          req = defaultRequest
                  `setPath` ("/customer/" <> BSL.toStrict cId <> ".json")
                  `addHeader` ("Accept", "*/*")
                  `addHeader` ("X-API-REVISION", "invalid")
      res <- request req
      assertStatus 422 res
      let expectedBody = "422 Unprocessable Entity: request header \"X-API-REVISION\" is invalid."
      assertBody expectedBody res

    it "GET /customer/:customerId returns application/x-www-form-urlencoded given application/x-www-form-urlencoded as the Accept header" . runStateTClientState $ do
      let cId = 1752
          cIdS = show cId
          req = defaultRequest
                  `setPath` ("/customer/" <> BS.pack cIdS)
                  `addHeader` ("Accept", "application/x-www-form-urlencoded")
      res <- request req
      assertStatus 200 res
      let expectedCustomer = Customer
            { customerId = cId
            , customerName = "Mr. " <> T.pack cIdS
            , customerApiVersion = Nothing
            }
      liftIO $ urlDecodeAsForm (simpleBody res) `shouldBe` Right expectedCustomer
      assertHeader "Content-Type" "application/x-www-form-urlencoded" res

    it "GET /customer/:customerId returns 406 given an unknown Accept header" . runStateTClientState $ do
      let cId = "1752"
          req = defaultRequest
                  `setPath` ("/customer/" <> BSL.toStrict cId)
                  `addHeader` ("Accept", "image/*")
      res <- request req
      assertStatus 406 res
      assertBody "406 Not Acceptable." res
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
              <> ",\"customerApiVersion\":null}"
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

    it "GET /customer/:customerId.txt-or-json returns a JSON string with Customer ID" . runStateTClientState $ do
      let cId = "1752"
          req = defaultRequest
                  `setPath` ("/customer/" <> TE.encodeUtf8 cId <> ".txt-or-json")
                  `addHeader` ("Accept", "application/json")
      res <- request req
      assertStatus 200 res
      let expectedBody = A.encode $ "Customer " <> cId
          expectedHeaders =
            [ ("Content-Type", "application/json")
            , ("X-RateLimit-Limit", "90")
            , ("X-RateLimit-Reset", "2023-06-21T00:14:24.5Z")
            ]

      assertBody expectedBody res
      assertHeaders expectedHeaders res

    it "GET /customer/503.txt-or-json returns an error message as a JSON string" . runStateTClientState $ do
      let cId = "503"
          req = defaultRequest
                  `setPath` ("/customer/" <> BSL.toStrict cId <> ".txt-or-json")
                  `addHeader` ("Accept", "application/json")
      res <- request req
      assertStatus 503 res
      let expectedBody = A.encode ("error" :: T.Text)
          expectedHeaders =
            [ ("Content-Type", "application/json")
            , ("X-ErrorId", "SERVER ERROR")
            ]

      assertBody expectedBody res
      assertHeaders expectedHeaders res

    it "GET /customer/:customerId.txt-or-json returns a plain text with Customer ID" . runStateTClientState $ do
      let cId = "1752"
          req = defaultRequest
                  `setPath` ("/customer/" <> BSL.toStrict cId <> ".txt-or-json")
                  `addHeader` ("Accept", "text/plain")
      res <- request req
      assertStatus 200 res
      let expectedBody = "Customer " <> cId
          expectedHeaders =
            [ ("Content-Type", "text/plain;charset=UTF-8")
            , ("X-RateLimit-Limit", "90")
            , ("X-RateLimit-Reset", "2023-06-21T00:14:24.5Z")
            ]

      assertBody expectedBody res
      assertHeaders expectedHeaders res

    it "GET /customer/503.txt-or-json returns an error message as a plain text" . runStateTClientState $ do
      let cId = "503"
          req = defaultRequest
                  `setPath` ("/customer/" <> BSL.toStrict cId <> ".txt-or-json")
                  `addHeader` ("Accept", "text/plain")
      res <- request req
      assertStatus 503 res
      let expectedBody = "error"
          expectedHeaders =
            [ ("Content-Type", "text/plain;charset=UTF-8")
            , ("X-ErrorId", "SERVER ERROR")
            ]

      assertBody expectedBody res
      assertHeaders expectedHeaders res

    it "GET /customerHeadered returns a customer's information with reponse headers" . runStateTClientState $ do
      let cId = "999"
          req = defaultRequest
                  `setPath` "/customerHeadered"
                  `addHeader` ("Accept", "text/html,*/*")
      res <- request req
      assertStatus 200 res
      let expectedBody =
            "{\"customerName\":\"Mr. " <> cId <> "\","
              <> "\"customerId\":"
              <> cId
              <> ",\"customerApiVersion\":null}"
          expectedHeaders =
            [ ("Content-Type", "application/json")
            , ("X-RateLimit-Limit", "50")
            , ("X-RateLimit-Reset", "2023-04-05T00:14:24.5Z")
            ]

      assertBody expectedBody res
      assertHeaders expectedHeaders res

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

    it "POST /echoApiVersion returns the value of X-API-VERSION request header" . runStateTClientState $ do
      let req = defaultRequest
                  { WaiI.requestMethod = methodPost }
                  `setPath` "/echoApiVersion/"
                  `addHeader` ("Accept", "*/*")
                  `addHeader` ("X-API-VERSION", BSL.toStrict expectedBody)
          expectedBody = "3333"
      res <- request req
      assertStatus 201 res
      assertBody expectedBody res

    it "POST /echoApiVersion returns the value of X-API-REVISION request header" . runStateTClientState $ do
      let req = defaultRequest
                  { WaiI.requestMethod = methodPost }
                  `setPath` "/echoApiVersion/"
                  `addHeader` ("Accept", "*/*")
                  `addHeader` ("X-API-REVISION", BSL.toStrict expectedBody)
          expectedBody = "4444"
      res <- request req
      assertStatus 201 res
      assertBody expectedBody res

    it "POST /echoApiVersion returns an 422 error not given either X-API-VERSION or X-API-REVISION request header" . runStateTClientState $ do
      let req = defaultRequest
                  { WaiI.requestMethod = methodPost }
                  `setPath` "/echoApiVersion/"
                  `addHeader` ("Accept", "*/*")
      res <- request req
      assertStatus 422 res
      let expectedBody = "422 Unprocessable Entity: Missing request header (one of [\"X-API-VERSION\",\"X-API-REVISION\"])"
      assertBody expectedBody res

    it "GET /exampleRequestHeaders returns the value of X-API-VERSION and X-API-KEY request headers" . runStateTClientState $ do
      let req = defaultRequest
                  `setPath` "/exampleRequestHeaders/"
                  `addHeader` ("Accept", "*/*")
                  `addHeader` ("X-API-VERSION", BS.pack . show $ unApiVersion expectedVersion)
                  `addHeader` ("X-API-KEY", TE.encodeUtf8 $ unWithRequestHeaderCodec expectedKey)
          expectedVersion = ApiVersion 3333
          expectedKey = "key 1"
          expectedBody = ExampleRequestHeaders
            { exampleRequestHeadersApiVersion = expectedVersion
            , exampleRequestHeadersApiKey = expectedKey
            }

      res <- request req
      assertStatus 200 res
      let expectedHeaders = [("Content-Type", "application/json")]

      assertHeaders expectedHeaders res
      liftIO $ A.decode (simpleBody res) `shouldBe` Just expectedBody

    it "GET /exampleRequestHeaders returns the value of X-API-REVISION and X-API-KEY request headers" . runStateTClientState $ do
      let req = defaultRequest
                  `setPath` "/exampleRequestHeaders/"
                  `addHeader` ("Accept", "*/*")
                  `addHeader` ("X-API-REVISION", BS.pack . show $ unApiVersion expectedVersion)
                  `addHeader` ("X-API-KEY", TE.encodeUtf8 $ unWithRequestHeaderCodec expectedKey)
          expectedVersion = ApiVersion 4444
          expectedKey = "key 2"
          expectedBody = ExampleRequestHeaders
            { exampleRequestHeadersApiVersion = expectedVersion
            , exampleRequestHeadersApiKey = expectedKey
            }

      res <- request req
      assertStatus 200 res
      let expectedHeaders = [("Content-Type", "application/json")]

      assertHeaders expectedHeaders res
      liftIO $ A.decode (simpleBody res) `shouldBe` Just expectedBody

    it "GET /exampleRequestHeaders returns an 422 error not given either X-API-VERSION or X-API-REVISION request header" . runStateTClientState $ do
      let req = defaultRequest
                  `setPath` "/exampleRequestHeaders/"
                  `addHeader` ("Accept", "*/*")
                  `addHeader` ("X-API-KEY", "key 3")
      res <- request req
      assertStatus 422 res
      let expectedBody = "422 Unprocessable Entity: Missing request header (one of [\"X-API-VERSION\",\"X-API-REVISION\"])"
      assertBody expectedBody res

    it "GET /exampleRequestHeaders returns an 422 error not given X-API-KEY request header" . runStateTClientState $ do
      let req = defaultRequest
                  `setPath` "/exampleRequestHeaders/"
                  `addHeader` ("Accept", "*/*")
                  `addHeader` ("X-API-VERSION", "9999")
      res <- request req
      assertStatus 422 res
      let expectedBody = "422 Unprocessable Entity: Missing request header \"X-API-KEY\""
      assertBody expectedBody res

assertHeaders :: HasCallStack => ResponseHeaders -> SResponse -> Session ()
assertHeaders expectedHeaders SResponse { simpleHeaders } =
  liftIO $ shouldMatchList expectedHeaders simpleHeaders


addHeader :: Wai.Request -> Header -> Wai.Request
addHeader r h = r { WaiI.requestHeaders = h : WaiI.requestHeaders r }


withWaiApp :: Wai.Application -> (Wai.Application -> IO ()) -> IO ()
withWaiApp app action = action app


runStateTClientState :: Session () -> ReaderT Wai.Application IO ()
runStateTClientState = mapReaderT (`evalStateT` initState)
