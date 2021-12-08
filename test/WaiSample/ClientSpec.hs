{-# LANGUAGE OverloadedStrings #-}

module WaiSample.ClientSpec
  ( spec
  ) where


import           Control.Concurrent       (forkIO, killThread)
import           Control.Exception        (bracket)
import qualified Data.Text                as T
import           Network.Wai.Handler.Warp (Port, run)
import           Test.QuickCheck          (property)
import           Test.Syd                 (Spec, aroundAll_, describe, it,
                                           shouldReturn)

import           WaiSample                (Customer (..), sampleApp)
import           WaiSample.Client         (httpConduitBackend)
import           WaiSample.Client.Sample


spec :: Spec
spec =
  aroundAll_ withServer $
    describe "WaiSample.Client.Sample" $ do
      let backend = httpConduitBackend $ "http://localhost:" ++ show port ++ "/"

      it "index returns \"index\"" $
        sampleIndex backend `shouldReturn` "index"

      it "aboutUs returns \"About IIJ\"" $
        sampleAboutUs backend `shouldReturn` "About IIJ"

      it "aboutUsFinance returns \"Financial Report 2021\"" $
        sampleAboutUsFinance backend `shouldReturn` "Financial Report 2021"

      it "aboutFinance returns \"Financial Report 2020 /\"" $
        sampleAboutFinance backend `shouldReturn` "Financial Report 2020 /"

      it "customerId returns a Customer object" $
        property $ \cId -> do
          let expected = Customer
                { customerName = "Mr. " <> T.pack (show cId)
                , customerId = cId
                }
          sampleCustomerId backend cId `shouldReturn` expected

      it "customerIdJson returns a Customer object" $
        property $ \cId -> do
          let expected = Customer
                { customerName = "Mr. " <> T.pack (show cId)
                , customerId = cId
                }
          sampleCustomerIdJson backend cId `shouldReturn` expected

      it "customerTransaction returns a transaction information" $
        property $ \(cId, tNameS) -> do
          let tName = T.pack tNameS
              expected =
                "Customer " <> T.pack (show cId) <> " Transaction " <> tName
          sampleCustomerTransaction backend cId tName `shouldReturn` expected


withServer :: IO () -> IO ()
withServer body =
  bracket
    (forkIO (run port sampleApp))
    killThread
    (\_ -> body)

-- TODO: decide port at random
port :: Port
port = 8020
