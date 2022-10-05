{-# LANGUAGE OverloadedStrings #-}

module WaiSample.ClientSpec
  ( spec
  ) where


import qualified Data.Text                as T
import           Network.HTTP.Client      (Manager, defaultManagerSettings,
                                           newManager)
import           Network.Wai.Handler.Warp (Port, testWithApplication)
import           Test.QuickCheck          (property)
import           Test.Syd                 (Spec, aroundAll, describe,
                                           itWithOuter, shouldReturn)

import           WaiSample                (Customer (..), sumLift)
import           WaiSample                (SampleError (SampleError))
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
                , customerId = cId
                }
          sampleCustomerIdJson backend cId `shouldReturn` sumLift expected

      -- TODO: customerIdJson should return an Error object
      -- TODO: customerIdTxt should return a plain text message
      -- TODO: customerIdTxt should return an error message

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


withServer :: (Port -> IO ()) -> IO ()
withServer = testWithApplication (return sampleApp)


withManager :: (Manager -> IO ()) -> IO ()
withManager action = action =<< newManager defaultManagerSettings


withManagerAndServer :: ((Manager, Port) -> IO ()) -> IO ()
withManagerAndServer action =
  withManager $ \manager ->
    withServer $ \port ->
      action (manager, port)
