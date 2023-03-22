{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

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
import           WaiSample.Types.Response.Headers
import           WaiSample.Types.Status

newtype Response resTyp resObj = Response
  { responseObject :: resObj
  } deriving (Show, Eq)

data RawResponse = RawResponse
  { rawStatusCode :: StatusCodeInfo
  , rawHeaders    :: HT.ResponseHeaders
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

class ResponseSpec resSpec => DecodeByResponseSpec resSpec where
  decodeByResponseSpec
    :: MediaType {- ^ Media type determined by 'matchAccept' with client's Accept header -}
    -> ResponseObject resSpec {- ^ Response Object: a value returned by the 'Handler' function. -}
    -> IO RawResponse

class ResponseSpec resSpec => EncodeByResponseSpec resSpec where
  encodeByResponseSpec
    :: MediaType {- ^ Media type returned by the server -}
    -> RawResponse {- ^ Response returned by the server whose body's type is unknown. -}
    -> IO (ResponseObject resSpec)


instance (ToJSON resObj, Typeable resObj) => DecodeByResponseSpec (Json, resObj) where
  decodeByResponseSpec mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . Json.encode

instance (ToJSON resObj, Typeable resObj) => DecodeByResponseSpec (Response Json resObj) where
  decodeByResponseSpec mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . Json.encode . responseObject

instance (FromJSON resObj, Typeable resObj) => EncodeByResponseSpec (Json, resObj) where
  encodeByResponseSpec _ = either fail return . Json.eitherDecode' . rawBody

instance (FromJSON resObj, Typeable resObj) => EncodeByResponseSpec (Response Json resObj) where
  encodeByResponseSpec _ = either fail (return . Response) . Json.eitherDecode' . rawBody


instance (ToForm resObj, Typeable resObj) => DecodeByResponseSpec (FormUrlEncoded, resObj) where
  decodeByResponseSpec mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . urlEncodeAsForm

instance (ToForm resObj, Typeable resObj) => DecodeByResponseSpec (Response FormUrlEncoded resObj) where
  decodeByResponseSpec mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . urlEncodeAsForm . responseObject

instance (FromForm resObj, Typeable resObj) => EncodeByResponseSpec (FormUrlEncoded, resObj) where
  encodeByResponseSpec _ = either (fail . T.unpack) return . urlDecodeAsForm . rawBody

instance (FromForm resObj, Typeable resObj) => EncodeByResponseSpec (Response FormUrlEncoded resObj) where
  encodeByResponseSpec _ = either (fail . T.unpack) (return . Response) . urlDecodeAsForm . rawBody


instance DecodeByResponseSpec (PlainText, T.Text) where
  decodeByResponseSpec mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . BL.fromStrict . TE.encodeUtf8

instance DecodeByResponseSpec (Response PlainText T.Text) where
  decodeByResponseSpec mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . BL.fromStrict . TE.encodeUtf8 . responseObject

instance EncodeByResponseSpec (PlainText, T.Text) where
  encodeByResponseSpec _ = return . TE.decodeUtf8 . BL.toStrict . rawBody

instance EncodeByResponseSpec (Response PlainText T.Text) where
  encodeByResponseSpec _ = return . Response . TE.decodeUtf8 . BL.toStrict . rawBody


instance {-# OVERLAPPING #-}
  ( Typeable resObj
  , HasContentTypes contTyp
  , DecodeByResponseSpec (contTyp, resObj)
  ) => DecodeByResponseSpec (ContentTypes '[contTyp], resObj) where
  decodeByResponseSpec mt resObj =
    if matchContentType @contTyp mt
      then
        decodeByResponseSpec @(contTyp, resObj) mt resObj
      else
        return $ RawResponse
          (NonDefaultStatus HT.status406)
          [(HT.hContentType, renderHeader mt)]
          (BL.pack "406 Not Acceptable")

instance
  ( Typeable resObj
  , HasContentTypes contTyp
  , HasContentTypes (ContentTypes contTyps)
  , DecodeByResponseSpec (contTyp, resObj)
  , DecodeByResponseSpec (ContentTypes contTyps, resObj)
  ) => DecodeByResponseSpec (ContentTypes (contTyp ': contTyps), resObj) where
  decodeByResponseSpec mt resObj =
    if matchContentType @contTyp mt
      then decodeByResponseSpec @(contTyp, resObj) mt resObj
      else decodeByResponseSpec @(ContentTypes contTyps, resObj) mt resObj

instance {-# OVERLAPPING #-}
  ( Typeable resObj
  , HasContentTypes contTyp
  , DecodeByResponseSpec (contTyp, resObj)
  ) => DecodeByResponseSpec (Response (ContentTypes '[contTyp]) resObj) where
  decodeByResponseSpec mt (Response resObj) = decodeByResponseSpec @(contTyp, resObj) mt resObj

instance
  ( Typeable resObj
  , HasContentTypes contTyp
  , HasContentTypes (ContentTypes contTyps)
  , DecodeByResponseSpec (contTyp, resObj)
  , DecodeByResponseSpec (ContentTypes contTyps, resObj)
  ) => DecodeByResponseSpec (Response (ContentTypes (contTyp ': contTyps)) resObj) where
  decodeByResponseSpec mt (Response resObj) = decodeByResponseSpec @(ContentTypes (contTyp ': contTyps), resObj) mt resObj


instance {-# OVERLAPPING #-}
  ( Typeable resObj
  , HasContentTypes contTyp
  , EncodeByResponseSpec (contTyp, resObj)
  ) => EncodeByResponseSpec (ContentTypes '[contTyp], resObj) where
  encodeByResponseSpec mt rr =
    if matchContentType @contTyp mt
      then encodeByResponseSpec @(contTyp, resObj) mt rr
      else fail "No content type matched"

instance
  ( Typeable resObj
  , HasContentTypes contTyp
  , HasContentTypes (ContentTypes contTyps)
  , EncodeByResponseSpec (contTyp, resObj)
  , EncodeByResponseSpec (ContentTypes contTyps, resObj)
  ) => EncodeByResponseSpec (ContentTypes (contTyp ': contTyps), resObj) where
  encodeByResponseSpec mt rr =
    if matchContentType @contTyp mt
      then encodeByResponseSpec @(contTyp, resObj) mt rr
      else encodeByResponseSpec @(ContentTypes contTyps, resObj) mt rr

instance {-# OVERLAPPING #-}
  ( Typeable resObj
  , HasContentTypes contTyp
  , EncodeByResponseSpec (contTyp, resObj)
  ) => EncodeByResponseSpec (Response (ContentTypes '[contTyp]) resObj) where
  encodeByResponseSpec mt rr = Response <$> encodeByResponseSpec @(contTyp, resObj) mt rr

instance
  ( Typeable resObj
  , HasContentTypes contTyp
  , HasContentTypes (ContentTypes contTyps)
  , EncodeByResponseSpec (contTyp, resObj)
  , EncodeByResponseSpec (ContentTypes contTyps, resObj)
  ) => EncodeByResponseSpec (Response (ContentTypes (contTyp ': contTyps)) resObj) where
  encodeByResponseSpec mt rr = Response <$> encodeByResponseSpec @(ContentTypes (contTyp ': contTyps), resObj) mt rr


class ToRawResponse resSpec where
  toRawResponse
    :: MediaType {- ^ Media type determined by 'matchAccept' with client's Accept header -}
    -> ResponseObject resSpec {- ^ Response Object: a value returned by the 'Handler' function. -}
    -> IO RawResponse

class FromRawResponse resSpec where
  fromRawResponse
    :: MediaType {- ^ Media type returned by the server -}
    -> RawResponse {- ^ Response returned by the server whose body's type is unknown. -}
    -> IO (ResponseObject resSpec)


instance {-# OVERLAPPABLE #-} DecodeByResponseSpec resSpec => ToRawResponse resSpec where
  toRawResponse = decodeByResponseSpec @resSpec

instance {-# OVERLAPPABLE #-} EncodeByResponseSpec resSpec => FromRawResponse resSpec where
  fromRawResponse = encodeByResponseSpec @resSpec

instance
  ( Typeable resObj
  , Typeable headers
  , HasContentTypes resTyp
  , UnwrapHeadered headers resObj
  , DecodeByResponseSpec (resTyp, resObj)
  ) => ToRawResponse (resTyp, Headered headers resObj) where
  toRawResponse mt hdResObj = do
    let (rawHeaders, resObj) = unwrapHeadered hdResObj
    rr <- decodeByResponseSpec @(resTyp, resObj) mt resObj
    return rr { rawHeaders }

instance
  ( Typeable resObj
  , Typeable headers
  , HasContentTypes resTyp
  , UnwrapHeadered headers resObj
  , DecodeByResponseSpec (resTyp, resObj)
  ) => ToRawResponse (Response resTyp (Headered headers resObj)) where
  toRawResponse mt (Response hdResObj) = toRawResponse @(resTyp, Headered headers resObj) mt hdResObj


instance
  ( Typeable resObj
  , Typeable headers
  , HasContentTypes resTyp
  , TryWrappingWithHeaders headers resObj
  , EncodeByResponseSpec (resTyp, resObj)
  ) => FromRawResponse (resTyp, Headered headers resObj) where
  fromRawResponse mt rr = do
    resObj <- encodeByResponseSpec @(resTyp, resObj) mt rr
    either fail return $ tryWrappingWithHeaders (rawHeaders rr) resObj

instance
  ( Typeable resObj
  , Typeable headers
  , HasContentTypes resTyp
  , TryWrappingWithHeaders headers resObj
  , EncodeByResponseSpec (resTyp, resObj)
  ) => FromRawResponse (Response resTyp (Headered headers resObj)) where
  fromRawResponse mt rr = Response <$> fromRawResponse @(resTyp, Headered headers resObj) mt rr
