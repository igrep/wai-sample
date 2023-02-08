{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module WaiSample.Types.Response where

import           Data.Aeson                       (FromJSON, ToJSON)
import qualified Data.Aeson                       as Json
import qualified Data.ByteString.Lazy.Char8       as BL
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Data.Typeable                    (Typeable)
import           Network.HTTP.Media               (MediaType)
import           Web.FormUrlEncoded               (FromForm, ToForm,
                                                   urlDecodeAsForm,
                                                   urlEncodeAsForm)

import           Network.HTTP.Media.RenderHeader  (renderHeader)
import qualified Network.HTTP.Types               as HT

import           WaiSample.Types.ContentTypes
import           WaiSample.Types.Response.Headers (Header, Headered)
import           WaiSample.Types.Status
import           Web.HttpApiData                  (ToHttpApiData)

newtype Response resTyp resObj = Response
  { responseObject :: resObj
  } deriving (Show, Eq)

data RawResponse = RawResponse
  { rawStatusCode :: StatusCodeInfo
  , rawHeader     :: HTS.ResponseHeaders
  , rawBody       :: BL.ByteString
  } deriving (Show, Eq)

defaultRawResponse :: HT.ResponseHeaders -> BL.ByteString -> RawResponse
defaultRawResponse = RawResponse DefaultStatus


class ResponseSpec resSpec where
  type ResponseType resSpec
  type ResponseObject resSpec

instance (HasStatusCode resTyp, HasContentTypes resTyp, Typeable resObj) => ResponseSpec (resTyp, resObj) where
  type ResponseType (resTyp, resObj) = resTyp
  type ResponseObject (resTyp, resObj) = resObj

instance (HasStatusCode resTyp, HasContentTypes resTyp, Typeable resObj) => ResponseSpec (Response resTyp resObj) where
  type ResponseType (Response resTyp resObj) = resTyp
  type ResponseObject (Response resTyp resObj) = Response resTyp resObj

class ResponseSpec resSpec => ToRawResponse resSpec where
  toRawResponse
    :: MediaType {- ^ Media type determined by 'matchAccept' with client's Accept header -}
    -> ResponseObject resSpec {- ^ Response Object: a value returned by the 'Handler' function. -}
    -> IO RawResponse

class ResponseSpec resSpec => FromRawResponse resSpec where
  fromRawResponse
    :: MediaType {- ^ Media type returned by the server -}
    -> RawResponse {- ^ Response returned by the server whose body's type is unknown. -}
    -> IO (ResponseObject resSpec)


instance (ToJSON resObj, Typeable resObj) => ToRawResponse (Json, resObj) where
  toRawResponse mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . Json.encode

instance (ToJSON resObj, Typeable resObj) => ToRawResponse (Response Json resObj) where
  toRawResponse mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . Json.encode . responseObject

instance (FromJSON resObj, Typeable resObj) => FromRawResponse (Json, resObj) where
  fromRawResponse _ = either fail return . Json.eitherDecode' . rawBody

instance (FromJSON resObj, Typeable resObj) => FromRawResponse (Response Json resObj) where
  fromRawResponse _ = either fail (return . Response) . Json.eitherDecode' . rawBody


instance (ToForm resObj, Typeable resObj) => ToRawResponse (FormUrlEncoded, resObj) where
  toRawResponse mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . urlEncodeAsForm

instance (ToForm resObj, Typeable resObj) => ToRawResponse (Response FormUrlEncoded resObj) where
  toRawResponse mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . urlEncodeAsForm . responseObject

instance (FromForm resObj, Typeable resObj) => FromRawResponse (FormUrlEncoded, resObj) where
  fromRawResponse _ = either (fail . T.unpack) return . urlDecodeAsForm . rawBody

instance (FromForm resObj, Typeable resObj) => FromRawResponse (Response FormUrlEncoded resObj) where
  fromRawResponse _ = either (fail . T.unpack) (return . Response) . urlDecodeAsForm . rawBody


instance ToRawResponse (PlainText, T.Text) where
  toRawResponse mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . BL.fromStrict . TE.encodeUtf8

instance ToRawResponse (Response PlainText T.Text) where
  toRawResponse mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . BL.fromStrict . TE.encodeUtf8 . responseObject

instance FromRawResponse (PlainText, T.Text) where
  fromRawResponse _ = return . TE.decodeUtf8 . BL.toStrict . rawBody

instance FromRawResponse (Response PlainText T.Text) where
  fromRawResponse _ = return . Response . TE.decodeUtf8 . BL.toStrict . rawBody


instance {-# OVERLAPPING #-}
  ( Typeable resObj
  , HasContentTypes contTyp
  , ToRawResponse (contTyp, resObj)
  ) => ToRawResponse (ContentTypes '[contTyp], resObj) where
  toRawResponse mt resObj =
    if matchContentType @contTyp mt
      then
        toRawResponse @(contTyp, resObj) mt resObj
      else
        return $ RawResponse
          (NonDefaultStatus HT.status406)
          [(HT.hContentType, renderHeader mt)]
          (BL.pack "406 Not Acceptable")

instance
  ( Typeable resObj
  , HasContentTypes contTyp
  , HasContentTypes (ContentTypes contTyps)
  , ToRawResponse (contTyp, resObj)
  , ToRawResponse (ContentTypes contTyps, resObj)
  ) => ToRawResponse (ContentTypes (contTyp ': contTyps), resObj) where
  toRawResponse mt resObj =
    if matchContentType @contTyp mt
      then toRawResponse @(contTyp, resObj) mt resObj
      else toRawResponse @(ContentTypes contTyps, resObj) mt resObj

instance {-# OVERLAPPING #-}
  ( Typeable resObj
  , HasContentTypes contTyp
  , ToRawResponse (contTyp, resObj)
  ) => ToRawResponse (Response (ContentTypes '[contTyp]) resObj) where
  toRawResponse mt (Response resObj) = toRawResponse @(contTyp, resObj) mt resObj

instance
  ( Typeable resObj
  , HasContentTypes contTyp
  , HasContentTypes (ContentTypes contTyps)
  , ToRawResponse (contTyp, resObj)
  , ToRawResponse (ContentTypes contTyps, resObj)
  ) => ToRawResponse (Response (ContentTypes (contTyp ': contTyps)) resObj) where
  toRawResponse mt (Response resObj) = toRawResponse @(ContentTypes (contTyp ': contTyps), resObj) mt resObj


instance {-# OVERLAPPING #-}
  ( Typeable resObj
  , HasContentTypes contTyp
  , FromRawResponse (contTyp, resObj)
  ) => FromRawResponse (ContentTypes '[contTyp], resObj) where
  fromRawResponse mt rr =
    if matchContentType @contTyp mt
      then fromRawResponse @(contTyp, resObj) mt rr
      else fail "No content type matched"

instance
  ( Typeable resObj
  , HasContentTypes contTyp
  , HasContentTypes (ContentTypes contTyps)
  , FromRawResponse (contTyp, resObj)
  , FromRawResponse (ContentTypes contTyps, resObj)
  ) => FromRawResponse (ContentTypes (contTyp ': contTyps), resObj) where
  fromRawResponse mt rr =
    if matchContentType @contTyp mt
      then fromRawResponse @(contTyp, resObj) mt rr
      else fromRawResponse @(ContentTypes contTyps, resObj) mt rr

instance {-# OVERLAPPING #-}
  ( Typeable resObj
  , HasContentTypes contTyp
  , FromRawResponse (contTyp, resObj)
  ) => FromRawResponse (Response (ContentTypes '[contTyp]) resObj) where
  fromRawResponse mt rr = Response <$> fromRawResponse @(contTyp, resObj) mt rr

instance
  ( Typeable resObj
  , HasContentTypes contTyp
  , HasContentTypes (ContentTypes contTyps)
  , FromRawResponse (contTyp, resObj)
  , FromRawResponse (ContentTypes contTyps, resObj)
  ) => FromRawResponse (Response (ContentTypes (contTyp ': contTyps)) resObj) where
  fromRawResponse mt rr = Response <$> fromRawResponse @(ContentTypes (contTyp ': contTyps), resObj) mt rr


instance {-# OVERLAPPING #-}
  ( Typeable resObj
  , ToHttpApiData hdObj
  , ToRawResponse resObj
  , ResponseSpec (resTyp, resObj)
  ) => ToRawResponse (resTyp, Headered '[Header name hdObj] resObj) where
  toRawResponse mt resObj = do
    rr <- toRawResponse mt resObj
    return rr { rawHeader = [undefined] }
