{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module WaiSample
  ( sampleRoutes
  , Customer (..)
  , SampleError (..)

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
import qualified Data.ByteString.Char8      as BS
import           Data.Proxy                 (Proxy (Proxy))
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Time                  (fromGregorian)
import           Data.Time.Clock            (UTCTime (UTCTime))
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import           Language.Haskell.TH.Syntax (Lift)
import           Network.HTTP.Types.Method  (Method, methodDelete, methodGet,
                                             methodPatch, methodPost, methodPut)
import           Web.FormUrlEncoded         (FromForm, ToForm)
import           Web.HttpApiData            (FromHttpApiData, ToHttpApiData)

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
  , get @(ContentTypes '[Json, FormUrlEncoded], Customer)
      "customerId"
      (path "customer/" *> decimalPiece)
      (return . customerOfId Nothing)
  , getWith @(Sum '[(Json, Customer), (WithStatus Status503 Json, SampleError)])
      "customerIdJson"
      -- /customer/:id.json
      -- TODO: optional (requestHeader "HEADER")と書いたとき、「"HEADER"をクライアントが送ってこなかった時」だけをNothingにすべき。"HEADER"の値が不正な文字列であった場合は、422 Unprocessable Entity（あるいはもっとふさわしいエラーがあればそれ）にすべき
      -- TODO: decimalPiece のパースに失敗したときは 404 Not Found
      -- TODO: requestHeader のパースに失敗したときは422 Unprocessable Entity（あるいはもっとふさわしいエラーがあればそれ）にするべき
      (path "customer/" *> (decimalPiece <* path ".json"))
      options
      { headersType = Proxy :: Proxy (Maybe ApiVersion)
      }
      (\i requestInfo -> do
        let apiVersion = requestHeadersValue requestInfo
        if i == 503
          then return . sumLift $ SampleError "Invalid Customer"
          else return . sumLift $ customerOfId apiVersion i
        )
  , get @(Sum '[(PlainText, T.Text), Response (WithStatus Status503 PlainText) T.Text])
      "customerIdTxt"
      -- /customer/:id.txt
      (path "customer/" *> decimalPiece <* path ".txt")
      (\i ->
        if i == 503
          then return . sumLift $ Response @(WithStatus Status503 PlainText) ("error" :: T.Text)
          else return . sumLift $ "Customer " <> T.pack (show i))
  , get @(PlainText, T.Text)
      "customerTransaction"
      ( (,) <$> (path "customer/" *> decimalPiece)
            <*> (path "/transaction/" *> paramPiece)
        )
      (\(cId, transactionName) ->
        return $ "Customer " <> T.pack (show cId) <> " Transaction " <> transactionName
        )
  , post @(PlainText, T.Text)
      "createProduct"
      (path "products")
      (\_ -> return ("Product created" :: T.Text))

  , get @(Json, Headered '[Header "X-RateLimit-Limit" Int, Header "X-RateLimit-Reset" UTCTime] Customer)
      "customerHeadered"
      (path "customerHeadered")
      (\_ -> do
        let exampleRateLimitReset = UTCTime (fromGregorian 2023 4 5) 864.5
        return . headered 50 . headered exampleRateLimitReset $ customerOfId Nothing 999
        )

  , get @(
      Sum
        '[
          ( ContentTypes '[PlainText, Json]
          , Headered '[Header "X-RateLimit-Limit" Int, Header "X-RateLimit-Reset" UTCTime] T.Text
          )
         , Response
            (WithStatus Status503 (ContentTypes '[Json, PlainText]))
            (Headered '[Header "X-ErrorId" T.Text] T.Text)
         ])
      "customerIdTxtHeadered"
      -- /customer/:id.txt-or-json
      (path "customer/" *> decimalPiece <* path ".txt-or-json")
      (\i -> do
        let time = UTCTime (fromGregorian 2023 6 21) 864.5
        if i == 503
          then
            return
              . sumLift
              $ Response
                @(WithStatus Status503 (ContentTypes '[Json, PlainText]))
                (headered @"X-ErrorId" ("SERVER ERROR" :: T.Text) ("error" :: T.Text))
          else
            return
              . sumLift
              . headered @"X-RateLimit-Limit" (90 :: Int)
              . headered @"X-RateLimit-Reset" time
              $ "Customer " <> T.pack (show i)
              )
  ]
 where
  customerOfId apiVersion i =
    Customer
      { customerName = "Mr. " <> T.pack (show i)
      , customerId = i
      , customerApiVersion = apiVersion
      }


data Customer = Customer
  { customerName       :: T.Text
  , customerId         :: Integer
  , customerApiVersion :: Maybe ApiVersion
  } deriving (Eq, Generic, Show, Lift)

instance ToJSON Customer where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance FromJSON Customer

instance ToForm Customer

instance FromForm Customer

newtype ApiVersion = ApiVersion Integer
  deriving stock (Eq, Generic, Show, Lift)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData)

instance ToRequestHeaders ApiVersion where
  toRequestHeaders (ApiVersion i) = [("X-API-VERSION", BS.pack (show i))]

instance FromRequestHeaders ApiVersion where
  fromRequestHeaders rhds =
    decodeHeader "X-API-VERSION" rhds `orHeader` decodeHeader "X-API-REVISION" rhds


newtype SampleError = SampleError
  { message :: String
  } deriving (Eq, Generic, Show, Lift)

instance ToJSON SampleError where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance FromJSON SampleError

instance ToForm SampleError

instance FromForm SampleError


showRoutes :: [Handler] -> T.Text
showRoutes = ("/" <>) . T.intercalate "\n/" . map f
 where
  f :: Handler -> T.Text
  f (Handler _resSpec _name _method tbl _opts _hdl) = showRoutes' tbl


handler
  :: forall resSpec a h.
  ( ToRawResponse resSpec
  , FromRawResponse resSpec
  , Typeable resSpec
  , Typeable (ResponseObject resSpec)
  , HasStatusCode (ResponseType resSpec)
  , HasContentTypes (ResponseType resSpec)
  , Typeable h
  , ToRequestHeaders h
  , FromRequestHeaders h
  )
  => String -> Method -> Route a -> EndpointOptions h -> Responder a h (ResponseObject resSpec) -> Handler
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
  => String -> Route a -> SimpleResponder a (ResponseObject resSpec) -> Handler
get name route respond   = getWith @resSpec @a name route options (\p _reqInfo -> respond p)
post name route respond   = postWith @resSpec @a name route options (\p _reqInfo -> respond p)
put name route respond    = putWith @resSpec @a name route options (\p _reqInfo -> respond p)
delete name route respond = deleteWith @resSpec @a name route options (\p _reqInfo -> respond p)
patch name  route respond = patchWith @resSpec @a name route options (\p _reqInfo -> respond p)


getWith, postWith, putWith, deleteWith, patchWith
  :: forall resSpec a h.
  ( ToRawResponse resSpec
  , FromRawResponse resSpec
  , Typeable resSpec
  , Typeable (ResponseObject resSpec)
  , HasStatusCode (ResponseType resSpec)
  , HasContentTypes (ResponseType resSpec)
  , Typeable h
  , ToRequestHeaders h
  , FromRequestHeaders h
  )
  => String -> Route a -> EndpointOptions h -> Responder a h (ResponseObject resSpec) -> Handler
getWith name    = handler @resSpec @a name methodGet
postWith name   = handler @resSpec @a name methodPost
putWith name    = handler @resSpec @a name methodPut
deleteWith name = handler @resSpec @a name methodDelete
patchWith name  = handler @resSpec @a name methodPatch


printRoutes :: IO ()
printRoutes = TIO.putStrLn $ showRoutes sampleRoutes
