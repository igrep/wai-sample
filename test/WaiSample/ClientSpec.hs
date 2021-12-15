{-# LANGUAGE OverloadedStrings #-}

module WaiSample.ClientSpec
  ( spec
  ) where


import qualified Data.Text                as T
import           Network.Wai.Handler.Warp (Port, testWithApplication)
import           Test.QuickCheck          (property)
import           Test.Syd                 (Spec, aroundAll, describe,
                                           itWithOuter, shouldReturn)

import           WaiSample                (Customer (..), sampleApp)
import           WaiSample.Client         (httpConduitBackend)
import           WaiSample.Client.Sample


spec :: Spec
spec =
  aroundAll withServer $
    describe "WaiSample.Client.Sample" $ do
      let buildBackend port = httpConduitBackend $ "http://localhost:" ++ show port ++ "/"

      itWithOuter "index returns \"index\"" $ \port -> do
        let backend = buildBackend port
        sampleIndex backend `shouldReturn` "index"

      itWithOuter "aboutUs returns \"About IIJ\"" $ \port -> do
        let backend = buildBackend port
        sampleAboutUs backend `shouldReturn` "About IIJ"

      itWithOuter "aboutUsFinance returns \"Financial Report 2021\"" $ \port -> do
        let backend = buildBackend port
        sampleAboutUsFinance backend `shouldReturn` "Financial Report 2021"

      itWithOuter "aboutFinance returns \"Financial Report 2020 /\"" $ \port -> do
        let backend = buildBackend port
        sampleAboutFinance backend `shouldReturn` "Financial Report 2020 /"

      itWithOuter "customerId returns a Customer object" $ \port -> do
        let backend = buildBackend port
        property $ \cId -> do
          let expected = Customer
                { customerName = "Mr. " <> T.pack (show cId)
                , customerId = cId
                }
          sampleCustomerId backend cId `shouldReturn` expected

      itWithOuter "customerIdJson returns a Customer object" $ \port -> do
        let backend = buildBackend port
        property $ \cId -> do
          let expected = Customer
                { customerName = "Mr. " <> T.pack (show cId)
                , customerId = cId
                }
          sampleCustomerIdJson backend cId `shouldReturn` expected

      itWithOuter "customerTransaction returns a transaction information" $ \port -> do
        let backend = buildBackend port
        property $ \(cId, tNameS) -> do
          let tName = T.pack tNameS
              expected =
                "Customer " <> T.pack (show cId) <> " Transaction " <> tName
          sampleCustomerTransaction backend cId tName `shouldReturn` expected


withServer :: (Port -> IO ()) -> IO ()
withServer = testWithApplication (return sampleApp)
