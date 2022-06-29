{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
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

  , getResponseObjectType
  , showRoutes
  , printRoutes
  ) where

import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Aeson                as Json
import           Data.Functor              (void)
import           Data.Proxy                (Proxy (Proxy))
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
import           Network.HTTP.Types.Method (Method, methodDelete, methodGet,
                                            methodPatch, methodPost, methodPut)
import           Web.FormUrlEncoded        (FromForm, ToForm)

import           WaiSample.Routes
import           WaiSample.Types


sampleRoutes :: [Handler]
sampleRoutes =
  [ get @PlainText "index" root (\_ -> return ("index" :: T.Text))
  , get @PlainText "maintenance" (path "maintenance")
      (\_ -> return $ Response Status503 ("Sorry, we are under maintenance" :: T.Text))
  , get @PlainText "aboutUs" (path "about/us") (\_ -> return ("About IIJ" :: T.Text))
  , get @PlainText "aboutUsFinance" (path "about/us/finance") (\_ -> return ("Financial Report 2021" :: T.Text))
  , get @PlainText "aboutFinance" (path "about/finance") (\_ -> return ("Financial Report 2020 /" :: T.Text))
  -- TODO: Drop the initial slash?
  , get @PlainText "aboutFinanceImpossible" (path "/about/finance/impossible") (\_ -> (fail "This should not be executed due to the leading slash" :: IO T.Text))
  , get @(ContentTypes '[Json, FormUrlEncoded]) "customerId"
      (path "customer/" *> decimalPiece)
      (return . customerOfId)
  -- TODO: try: get @(Sum '[Json, WithStatus Status503 (ContentTypes '[Json, FormUrlEncoded])]) @(Sum '[Customer, SampleError]) "customerIdJson"
  , get @(Sum '[Json, WithStatus Status503 Json]) @(Sum '[Customer, SampleError]) "customerIdJson"
    -- /customer/:id.json
      (path "customer/" *> decimalPiece <* path ".json")
      (\i ->
        if i == 503
          then return . sumLift $ SampleError "Invalid Customer"
          else return . sumLift $ customerOfId i)
  , get @PlainText "customerTransaction"
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
  , post @PlainText "createProduct"
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
  } deriving (Eq, Generic, Show)

instance ToJSON Customer where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance FromJSON Customer

instance ToForm Customer

instance FromForm Customer


newtype SampleError = SampleError
  { message :: String
  } deriving (Eq, Generic, Show)

instance ToJSON SampleError where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance FromJSON SampleError

instance ToForm SampleError

instance FromForm SampleError


getResponseObjectType :: (a -> IO resObj) -> Proxy resObj
getResponseObjectType _ = Proxy


showRoutes :: [Handler] -> T.Text
showRoutes = ("/" <>) . T.intercalate "\n/" . map (showRoutes' . extractRoutingTable)

extractRoutingTable :: Handler -> RoutingTable ()
extractRoutingTable (Handler _name _method tbl _hdl) = void tbl


handler
  :: forall resTyp resObj a.
  ( ToRawResponse resTyp resObj
  , FromRawResponse resTyp resObj
  )
  => String -> Method -> RoutingTable a -> (a -> IO resObj) -> Handler
handler = Handler @resTyp @resObj @a


get, post, put, delete, patch
  :: forall resTyp resObj a.
  ( ToRawResponse resTyp resObj
  , FromRawResponse resTyp resObj
  )
  => String -> RoutingTable a -> (a -> IO resObj) -> Handler
get name    = handler @resTyp @resObj @a name methodGet
post name   = handler @resTyp @resObj @a name methodPost
put name    = handler @resTyp @resObj @a name methodPut
delete name = handler @resTyp @resObj @a name methodDelete
patch name  = handler @resTyp @resObj @a name methodPatch


printRoutes :: IO ()
printRoutes = TIO.putStrLn $ showRoutes sampleRoutes
