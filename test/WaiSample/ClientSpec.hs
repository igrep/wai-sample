{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module WaiSample.ClientSpec
  ( spec
  ) where


import qualified Data.Text                as T
import           Data.Time                (fromGregorian)
import           Data.Time.Clock          (UTCTime (UTCTime))
import           Network.HTTP.Client      (Manager, defaultManagerSettings,
                                           newManager)
import           Network.Wai.Handler.Warp (Port, testWithApplication)
import           Test.QuickCheck          (property)
import           Test.Syd                 (Spec, aroundAll, describe,
                                           itWithOuter, shouldReturn)

import           WaiSample                (ContentTypes, Customer (..), Json,
                                           PlainText, Response (Response),
                                           SampleError (SampleError), Status503,
                                           WithStatus, headered, sumLift)
import           WaiSample.Client         (httpClientBackend)
import           WaiSample.Client.Sample
import           WaiSample.Server         (sampleApp)


spec :: Spec
spec =
  aroundAll withManagerAndServer $
    describe "WaiSample.Client.Sample" $ do
      let buildBackend port = httpClientBackend $ "http://localhost:" ++ show port ++ "/"

      itWithOuter "index returns \"index\"" $ \(manager, port) -> do
        let backend = buildBackend port manager
        sampleIndex backend `shouldReturn` "index"

      itWithOuter "sampleMaintenance returns \"Sorry, we are under maintenance\"" $ \(manager, port) -> do
        let backend = buildBackend port manager
        sampleMaintenance backend `shouldReturn` "Sorry, we are under maintenance"

      itWithOuter "aboutUs returns \"About IIJ\"" $ \(manager, port) -> do
        let backend = buildBackend port manager
        sampleAboutUs backend `shouldReturn` "About IIJ"

      itWithOuter "aboutUsFinance returns \"Financial Report 2021\"" $ \(manager, port) -> do
        let backend = buildBackend port manager
        sampleAboutUsFinance backend `shouldReturn` "Financial Report 2021"

      itWithOuter "aboutFinance returns \"Financial Report 2020 /\"" $ \(manager, port) -> do
        let backend = buildBackend port manager
        sampleAboutFinance backend `shouldReturn` "Financial Report 2020 /"

      itWithOuter "customerId returns a Customer object" $ \(manager, port) -> do
        let backend = buildBackend port manager
        property $ \cId -> do
          let expected = Customer
                { customerName = "Mr. " <> T.pack (show cId)
                , customerId = cId
                }
          sampleCustomerId backend cId `shouldReturn` expected

      itWithOuter "customerIdJson returns a Customer object" $ \(manager, port) -> do
        let backend = buildBackend port manager
        property $ \cId -> do
          let expected = Customer
                { customerName = "Mr. " <> T.pack (show cId)
                -- `customerIdJson` returns an Error if cId is 503 as the next test shows.
                , customerId = if cId == 503 then 504 else cId
                }
          sampleCustomerIdJson backend cId `shouldReturn` sumLift expected

      itWithOuter "customerIdJson returns an error object if customerId is 503" $ \(manager, port) -> do
        let backend = buildBackend port manager
            expected = SampleError "Invalid Customer"
        sampleCustomerIdJson backend 503 `shouldReturn` sumLift expected

      itWithOuter "customerIdTxt returns the Customer ID" $ \(manager, port) -> do
        let backend = buildBackend port manager
        property $ \cId0 -> do
          let cId = if cId0 == 503 then 504 else cId0
              expected = "Customer " <> T.pack (show cId)
          sampleCustomerIdTxt backend cId `shouldReturn` sumLift expected

      itWithOuter "customerIdTxt returns an error message if customerId is 503" $ \(manager, port) -> do
        let backend = buildBackend port manager
            expected = Response @(WithStatus Status503 PlainText) ("error" :: T.Text)
        sampleCustomerIdTxt backend 503 `shouldReturn` sumLift expected

      itWithOuter "customerTransaction returns a transaction information" $ \(manager, port) -> do
        let backend = buildBackend port manager
        property $ \(cId, tNameS) -> do
          let tName = T.pack tNameS
              expected =
                "Customer " <> T.pack (show cId) <> " Transaction " <> tName
          sampleCustomerTransaction backend cId tName `shouldReturn` expected

      itWithOuter "createProduct returns \"Product created\"" $ \(manager, port) -> do
        let backend = buildBackend port manager
        sampleCreateProduct backend `shouldReturn` "Product created"

      itWithOuter "customerHeadered returns a customer with \"X-RateLimit-*\" response header" $ \(manager, port) -> do
        let backend = buildBackend port manager
            exampleRateLimitReset = UTCTime (fromGregorian 2023 4 5) 864.5
            cId = 999
            expected = headered 50 . headered exampleRateLimitReset $ Customer
                { customerName = "Mr. " <> T.pack (show cId)
                , customerId = cId
                }
        sampleCustomerHeadered backend `shouldReturn` expected

      itWithOuter "customerIdTxtHeadered returns a customer ID with \"X-RateLimit-*\" response header" $ \(manager, port) -> do
        let backend = buildBackend port manager
            exampleRateLimitReset = UTCTime (fromGregorian 2023 6 21) 864.5
            cId = 200
            expected = sumLift
              . headered @"X-RateLimit-Limit" (90 :: Int)
              . headered @"X-RateLimit-Reset" exampleRateLimitReset
              $ "Customer " <> T.pack (show cId)
        sampleCustomerIdTxtHeadered backend cId `shouldReturn` expected

      itWithOuter "customerIdTxtHeadered returns an error given a customer ID 503" $ \(manager, port) -> do
        let backend = buildBackend port manager
            cId = 503
            expected = sumLift
              $ Response
                @(WithStatus Status503 (ContentTypes '[Json, PlainText]))
                (headered @"X-ErrorId" ("SERVER ERROR" :: T.Text) ("error" :: T.Text))
        sampleCustomerIdTxtHeadered backend cId `shouldReturn` expected


withServer :: (Port -> IO ()) -> IO ()
withServer = testWithApplication (return sampleApp)


withManager :: (Manager -> IO ()) -> IO ()
withManager action = action =<< newManager defaultManagerSettings


withManagerAndServer :: ((Manager, Port) -> IO ()) -> IO ()
withManagerAndServer action =
  withManager $ \manager ->
    withServer $ \port ->
      action (manager, port)
