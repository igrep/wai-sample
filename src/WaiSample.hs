{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveLift                #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

module WaiSample
  ( sampleRoutes
  , Customer (..)

  , root
  , path
  , paramPiece
  , decimalPiece
  , Handler (..)
  , handler

  , get
  , post
  , put
  , delete
  , patch

  , module WaiSample.Types

  , showRoutes
  , printRoutes
  ) where

import           Data.Aeson                 (FromJSON, ToJSON)
import qualified Data.Aeson                 as Json
import           Data.Functor               (void)
import           Data.Proxy                 (Proxy (Proxy))
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           GHC.Generics               (Generic)
import           Language.Haskell.TH.Syntax (Lift)
import           Network.HTTP.Types.Method  (Method, methodDelete, methodGet,
                                             methodPatch, methodPost, methodPut)
import           Web.FormUrlEncoded         (FromForm, ToForm)

import           Data.Typeable              (Typeable)
import           WaiSample.Routes
import           WaiSample.Types


sampleRoutes :: [Handler]
sampleRoutes =
  [ get @(PlainText, T.Text) "index" root (\_ -> return "index")
  , get @(WithStatus Status503 PlainText, T.Text) "maintenance" (path "maintenance")
      (\_ -> return "Sorry, we are under maintenance")
  , get @(PlainText, T.Text) "aboutUs" (path "about/us") (\_ -> return "About IIJ")
  , get @(PlainText, T.Text) "aboutUsFinance" (path "about/us/finance") (\_ -> return "Financial Report 2021")
  , get @(PlainText, T.Text) "aboutFinance" (path "about/finance") (\_ -> return "Financial Report 2020 /")
  -- TODO: Drop the initial slash?
  , get @(PlainText, T.Text) "aboutFinanceImpossible" (path "/about/finance/impossible") (\_ -> fail "This should not be executed due to the leading slash")
  , get @(ContentTypes '[Json, FormUrlEncoded], Customer) "customerId"
      (path "customer/" *> decimalPiece)
      (return . customerOfId)
  , get @(Sum '[(Json, Customer), (WithStatus Status503 Json, SampleError)]) "customerIdJson"
    -- /customer/:id.json
      (path "customer/" *> decimalPiece <* path ".json")
      (\i ->
        if i == 503
          then return . sumLift $ SampleError "Invalid Customer"
          else return . sumLift $ customerOfId i)
  , get @(Sum '[(PlainText, T.Text), Response (WithStatus Status503 PlainText) T.Text]) "customerIdTxt"
    -- /customer/:id.json
      (path "customer/" *> decimalPiece <* path ".txt")
      (\i ->
        if i == 503
          then return . sumLift $ Response @(WithStatus Status503 PlainText) ("error" :: T.Text)
          else return . sumLift $ "Customer " <> T.pack (show i))
  , get @(PlainText, T.Text) "customerTransaction"
    ( do
        path "customer/"
        cId <- decimalPiece
        path "/transaction/"
        transactionName <- paramPiece
        pure (cId, transactionName)
      )
    (\(cId, transactionName) ->
      return $ "Customer " <> T.pack (show cId) <> " Transaction " <> transactionName
      )
  , post @(PlainText, T.Text) "createProduct"
      (path "products")
      (\_ -> return ("Product created" :: T.Text))
  ]
 where
  customerOfId i =
    Customer
      { customerName = "Mr. " <> T.pack (show i)
      , customerId = i
      }


data Customer = Customer
  { customerName :: T.Text
  , customerId   :: Integer
  } deriving (Eq, Generic, Show, Lift)

instance ToJSON Customer where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance FromJSON Customer

instance ToForm Customer

instance FromForm Customer


newtype SampleError = SampleError
  { message :: String
  } deriving (Eq, Generic, Show, Lift)

instance ToJSON SampleError where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance FromJSON SampleError

instance ToForm SampleError

instance FromForm SampleError


showRoutes :: [Handler] -> T.Text
showRoutes = ("/" <>) . T.intercalate "\n/" . map (showRoutes' . extractRoutingTable)

extractRoutingTable :: Handler -> RoutingTable ()
extractRoutingTable (Handler _resSpec _name _method tbl _hdl) = void tbl


handler
  :: forall resSpec a.
  ( ToRawResponse resSpec
  , FromRawResponse resSpec
  , Typeable resSpec
  , Typeable (ResponseObject resSpec)
  , HasStatusCode (ResponseType resSpec)
  , HasContentTypes (ResponseType resSpec)
  )
  => String -> Method -> RoutingTable a -> (a -> IO (ResponseObject resSpec)) -> Handler
handler = Handler (Proxy :: Proxy resSpec)


get, post, put, delete, patch
  :: forall resSpec a.
  ( ToRawResponse resSpec
  , FromRawResponse resSpec
  , Typeable resSpec
  , Typeable (ResponseObject resSpec)
  , HasStatusCode (ResponseType resSpec)
  , HasContentTypes (ResponseType resSpec)
  )
  => String -> RoutingTable a -> (a -> IO (ResponseObject resSpec)) -> Handler
get name    = handler @resSpec @a name methodGet
post name   = handler @resSpec @a name methodPost
put name    = handler @resSpec @a name methodPut
delete name = handler @resSpec @a name methodDelete
patch name  = handler @resSpec @a name methodPatch


printRoutes :: IO ()
printRoutes = TIO.putStrLn $ showRoutes sampleRoutes
