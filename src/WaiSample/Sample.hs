{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module WaiSample.Sample where

import           Data.Aeson                 (FromJSON, ToJSON, toEncoding)
import qualified Data.Aeson                 as Json
import           Data.Aeson.DeriveNoPrefix  (deriveJsonNoTypeNamePrefix)
import           Data.Coerce                (Coercible)
import           Data.Proxy                 (Proxy (Proxy))
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Time                  (fromGregorian)
import           Data.Time.Clock            (UTCTime (UTCTime))
import           GHC.Generics               (Generic)
import           GHC.Prim                   (coerce)
import           Language.Haskell.TH.Syntax (Lift)
import           Web.FormUrlEncoded         (FromForm, ToForm)
import           Web.HttpApiData            (FromHttpApiData, ToHttpApiData,
                                             parseUrlPiece, toUrlPiece)

import           WaiSample


data Customer = Customer
  { customerName       :: T.Text
  , customerId         :: Integer
  , customerApiVersion :: Maybe (ApiVersion (WithRequestHeaderCodec "X-API-VERSION") (WithRequestHeaderCodec "X-API-REVISION"))
  } deriving (Eq, Generic, Show, Lift)

instance ToJSON Customer where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance FromJSON Customer

instance ToForm Customer

instance FromForm Customer


data ApiVersion codecV codecR =
    ApiVersion (codecV Integer)
  | ApiRevision (codecR Integer)
  deriving stock Generic

deriving instance (Eq (codecV Integer), Eq (codecR Integer)) => Eq (ApiVersion codecV codecR)
deriving instance (Show (codecV Integer), Show (codecR Integer)) => Show (ApiVersion codecV codecR)
deriving instance (Lift (codecV Integer), Lift (codecR Integer)) => Lift (ApiVersion codecV codecR)

instance (Coercible (codecV Integer) Integer, Coercible (codecR Integer) Integer) => ToHttpApiData (ApiVersion codecV codecR) where
  toUrlPiece (ApiVersion i)  = T.pack $ show (coerce i :: Integer)
  toUrlPiece (ApiRevision i) = T.pack $ show (coerce i :: Integer)

instance Coercible Integer (codecV Integer) => FromHttpApiData (ApiVersion codecV codecR) where
  parseUrlPiece =
    ApiVersion . coerce @Integer <$> parseUrlPiece

instance (Coercible (codecV Integer) Integer, Coercible (codecR Integer) Integer) => ToJSON (ApiVersion codecV codecR) where
  toJSON (ApiVersion i)  = Json.toJSON (coerce i :: Integer)
  toJSON (ApiRevision i) = Json.toJSON (coerce i :: Integer)

  toEncoding (ApiVersion i)  = Json.toEncoding (coerce i :: Integer)
  toEncoding (ApiRevision i) = Json.toEncoding (coerce i :: Integer)

instance Coercible Integer (codecV Integer) => FromJSON (ApiVersion codecV codecR) where
  parseJSON =
    fmap (ApiVersion . coerce @Integer) . Json.parseJSON

instance ToRequestHeaders (ApiVersion (WithRequestHeaderCodec "X-API-VERSION") (WithRequestHeaderCodec "X-API-REVISION"))
instance FromRequestHeaders (ApiVersion (WithRequestHeaderCodec "X-API-VERSION") (WithRequestHeaderCodec "X-API-REVISION"))
instance ShowRequestHeadersType (ApiVersion (WithRequestHeaderCodec "X-API-VERSION") (WithRequestHeaderCodec "X-API-REVISION"))

instance ToQueryParams (ApiVersion (WithQueryParamCodec "apiVersion") (WithQueryParamCodec "apiRevision"))
instance FromQueryParams (ApiVersion (WithQueryParamCodec "apiVersion") (WithQueryParamCodec "apiRevision"))
instance ShowQueryParamsType (ApiVersion (WithQueryParamCodec "apiVersion") (WithQueryParamCodec "apiRevision"))


apiVersion :: (Coercible Integer (codecV Integer), Coercible Integer (codecR Integer)) => Integer -> ApiVersion codecV codecR
apiVersion = ApiVersion . coerce

unApiVersion :: (Coercible (codecV Integer) Integer, Coercible (codecR Integer) Integer) => ApiVersion codecV codecR -> Integer
unApiVersion (ApiVersion i)  = coerce i
unApiVersion (ApiRevision i) = coerce i


data ExampleRequestHeaders = ExampleRequestHeaders
  { exampleRequestHeadersApiVersion :: ApiVersion (WithRequestHeaderCodec "X-API-VERSION") (WithRequestHeaderCodec "X-API-REVISION")
  , exampleRequestHeadersApiKey     :: WithRequestHeaderCodec "X-API-KEY" T.Text
  } deriving stock (Eq, Show, Generic, Lift)

$(deriveJsonNoTypeNamePrefix ''ExampleRequestHeaders)

instance ToRequestHeaders ExampleRequestHeaders
instance FromRequestHeaders ExampleRequestHeaders
instance ShowRequestHeadersType ExampleRequestHeaders


data ExampleQueryParams = ExampleQueryParams
  { exampleQueryParamsApiVersion :: ApiVersion (WithQueryParamCodec "apiVersion") (WithQueryParamCodec "apiRevision")
  , exampleQueryParamsApiKey     :: WithQueryParamCodec "apiKey" T.Text
  } deriving stock (Eq, Show, Generic, Lift)

$(deriveJsonNoTypeNamePrefix ''ExampleQueryParams)

instance ToQueryParams ExampleQueryParams
instance FromQueryParams ExampleQueryParams
instance ShowQueryParamsType ExampleQueryParams

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
      (path "customer/" *> decimalPiece <* path ".json")
      options
      { requestHeadersType = Proxy :: Proxy (Maybe (ApiVersion (WithRequestHeaderCodec "X-API-VERSION") (WithRequestHeaderCodec "X-API-REVISION")))
      }
      (\i requestInfo -> do
        let apiVer = requestHeadersValue requestInfo
        if i == 503
          then return . sumLift $ SampleError "Invalid Customer"
          else return . sumLift $ customerOfId apiVer i
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

  , postWith @(Json, ApiVersion (WithRequestHeaderCodec "X-API-VERSION") (WithRequestHeaderCodec "X-API-REVISION"))
      "echoApiVersion"
      -- /echoApiVersion
      (path "echoApiVersion/")
      options
      { requestHeadersType = Proxy :: Proxy (ApiVersion (WithRequestHeaderCodec "X-API-VERSION") (WithRequestHeaderCodec "X-API-REVISION"))
      }
      (\_ requestInfo -> return $ requestHeadersValue requestInfo)

  , getWith @(Json, ExampleRequestHeaders)
      "getExampleRequestHeaders"
      -- /exampleRequestHeaders
      (path "exampleRequestHeaders/")
      options
      { requestHeadersType = Proxy :: Proxy ExampleRequestHeaders
      }
      (\_ requestInfo -> return $ requestHeadersValue requestInfo)

  , postWith @(Json, ApiVersion (WithQueryParamCodec "apiVersion") (WithQueryParamCodec "apiRevision"))
      "echoApiVersionQ"
      (path "echoApiVersionQ/")
      options
      { queryParamsType = Proxy :: Proxy (ApiVersion (WithQueryParamCodec "apiVersion") (WithQueryParamCodec "apiRevision"))
      }
      (\_ requestInfo -> return $ queryParamsValue requestInfo)

  , getWith @(Json, ExampleQueryParams)
      "getExampleQueryParams"
      (path "exampleQueryParams/")
      options
      { queryParamsType = Proxy :: Proxy ExampleQueryParams
      }
      (\_ requestInfo -> return $ queryParamsValue requestInfo)
  ]
 where
  customerOfId apiVer i =
    Customer
      { customerName = "Mr. " <> T.pack (show i)
      , customerId = i
      , customerApiVersion = apiVer
      }


printRoutes :: IO ()
printRoutes = TIO.putStrLn $ showRoutes sampleRoutes


newtype SampleError = SampleError
  { message :: String
  } deriving (Eq, Generic, Show, Lift)

instance ToJSON SampleError where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance FromJSON SampleError

instance ToForm SampleError

instance FromForm SampleError
