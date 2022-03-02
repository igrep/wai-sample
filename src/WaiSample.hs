{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

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
  -- get "index" root (WithStatus status505 Json :<|> WithStatus status500 PlainText)) (\_ -> return $ body (Right "index" :: Either Error T.Text))
  [ get "index" root PlainText (\_ -> return ("index" :: T.Text))
  , get "maintenance" (path "maintenance")
      (WithStatus Status503 PlainText)
      (\_ -> return $ Response Status503 ("Sorry, we are under maintenance" :: T.Text))
  , get "aboutUs" (path "about/us") PlainText (\_ -> return ("About IIJ" :: T.Text))
  , get "aboutUsFinance" (path "about/us/finance") PlainText (\_ -> return ("Financial Report 2021" :: T.Text))
  , get "aboutFinance" (path "about/finance") PlainText (\_ -> return ("Financial Report 2020 /" :: T.Text))
  -- TODO: Drop the initial slash?
  , get "aboutFinanceImpossible" (path "/about/finance/impossible") PlainText (\_ -> (fail "This should not be executed due to the leading slash" :: IO T.Text))
  , get "customerId"
      (path "customer/" *> decimalPiece)
      (Json :<|> FormUrlEncoded)
      (return . customerOfId)
  , get "customerIdJson"
    -- /customer/:id.json
    (path "customer/" *> decimalPiece <* path ".json")
    Json
    (return . customerOfId)
  , get "customerTransaction"
    ( do
        path "customer/"
        cId <- decimalPiece
        path "/transaction/"
        transactionName <- paramPiece
        pure (cId, transactionName)
      )
    PlainText
    (\(cId, transactionName) ->
      return $ "Customer " <> T.pack (show cId) <> " Transaction " <> transactionName
      )
  , post "createProduct"
      (path "products")
      PlainText
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


getResponseObjectType :: (a -> IO resObj) -> Proxy resObj
getResponseObjectType _ = Proxy


showRoutes :: [Handler] -> T.Text
showRoutes = ("/" <>) . T.intercalate "\n/" . map (showRoutes' . extractRoutingTable)

extractRoutingTable :: Handler -> RoutingTable ()
extractRoutingTable (Handler _name _method tbl _ctype _hdl) = void tbl


handler
  :: forall a resTyp resObj.
  ( Typeable a
  , HasContentTypes resTyp
  , HasStatusCode resTyp
  , ToRawResponse resTyp resObj
  , FromRawResponse resTyp resObj
  )
  => String -> Method -> RoutingTable a -> resTyp -> (a -> IO resObj) -> Handler
handler = Handler


get, post, put, delete, patch
  :: forall a resTyp resObj.
  ( Typeable a
  , HasContentTypes resTyp
  , HasStatusCode resTyp
  , ToRawResponse resTyp resObj
  , FromRawResponse resTyp resObj
  )
  => String -> RoutingTable a -> resTyp -> (a -> IO resObj) -> Handler
get name    = handler name methodGet
post name   = handler name methodPost
put name    = handler name methodPut
delete name = handler name methodDelete
patch name  = handler name methodPatch


printRoutes :: IO ()
printRoutes = TIO.putStrLn $ showRoutes sampleRoutes
