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

class ResponseSpec resSpec => DecodeByMimeType resSpec where
  decodeByMimeType
    :: MediaType {- ^ Media type determined by 'matchAccept' with client's Accept header -}
    -> ResponseObject resSpec {- ^ Response Object: a value returned by the 'Handler' function. -}
    -> IO RawResponse

class ResponseSpec resSpec => EncodeByResponseSpec resSpec where
  encodeByResponseSpec
    :: MediaType {- ^ Media type returned by the server -}
    -> RawResponse {- ^ Response returned by the server whose body's type is unknown. -}
    -> IO (ResponseObject resSpec)


instance (ToJSON resObj, Typeable resObj) => DecodeByMimeType (Json, resObj) where
  decodeByMimeType mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . Json.encode

instance (ToJSON resObj, Typeable resObj) => DecodeByMimeType (Response Json resObj) where
  decodeByMimeType mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . Json.encode . responseObject

instance (FromJSON resObj, Typeable resObj) => EncodeByResponseSpec (Json, resObj) where
  encodeByResponseSpec _ = either fail return . Json.eitherDecode' . rawBody

instance (FromJSON resObj, Typeable resObj) => EncodeByResponseSpec (Response Json resObj) where
  encodeByResponseSpec _ = either fail (return . Response) . Json.eitherDecode' . rawBody


instance (ToForm resObj, Typeable resObj) => DecodeByMimeType (FormUrlEncoded, resObj) where
  decodeByMimeType mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . urlEncodeAsForm

instance (ToForm resObj, Typeable resObj) => DecodeByMimeType (Response FormUrlEncoded resObj) where
  decodeByMimeType mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . urlEncodeAsForm . responseObject

instance (FromForm resObj, Typeable resObj) => EncodeByResponseSpec (FormUrlEncoded, resObj) where
  encodeByResponseSpec _ = either (fail . T.unpack) return . urlDecodeAsForm . rawBody

instance (FromForm resObj, Typeable resObj) => EncodeByResponseSpec (Response FormUrlEncoded resObj) where
  encodeByResponseSpec _ = either (fail . T.unpack) (return . Response) . urlDecodeAsForm . rawBody


instance DecodeByMimeType (PlainText, T.Text) where
  decodeByMimeType mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . BL.fromStrict . TE.encodeUtf8

instance DecodeByMimeType (Response PlainText T.Text) where
  decodeByMimeType mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . BL.fromStrict . TE.encodeUtf8 . responseObject

instance EncodeByResponseSpec (PlainText, T.Text) where
  encodeByResponseSpec _ = return . TE.decodeUtf8 . BL.toStrict . rawBody

instance EncodeByResponseSpec (Response PlainText T.Text) where
  encodeByResponseSpec _ = return . Response . TE.decodeUtf8 . BL.toStrict . rawBody


instance {-# OVERLAPPING #-}
  ( Typeable resObj
  , HasContentTypes contTyp
  , DecodeByMimeType (contTyp, resObj)
  ) => DecodeByMimeType (ContentTypes '[contTyp], resObj) where
  decodeByMimeType mt resObj =
    if matchContentType @contTyp mt
      then
        decodeByMimeType @(contTyp, resObj) mt resObj
      else
        return $ RawResponse
          (NonDefaultStatus HT.status406)
          [(HT.hContentType, renderHeader mt)]
          (BL.pack "406 Not Acceptable")

instance
  ( Typeable resObj
  , HasContentTypes contTyp
  , HasContentTypes (ContentTypes contTyps)
  , DecodeByMimeType (contTyp, resObj)
  , DecodeByMimeType (ContentTypes contTyps, resObj)
  ) => DecodeByMimeType (ContentTypes (contTyp ': contTyps), resObj) where
  decodeByMimeType mt resObj =
    if matchContentType @contTyp mt
      then decodeByMimeType @(contTyp, resObj) mt resObj
      else decodeByMimeType @(ContentTypes contTyps, resObj) mt resObj

instance {-# OVERLAPPING #-}
  ( Typeable resObj
  , HasContentTypes contTyp
  , DecodeByMimeType (contTyp, resObj)
  ) => DecodeByMimeType (Response (ContentTypes '[contTyp]) resObj) where
  decodeByMimeType mt (Response resObj) = decodeByMimeType @(contTyp, resObj) mt resObj

instance
  ( Typeable resObj
  , HasContentTypes contTyp
  , HasContentTypes (ContentTypes contTyps)
  , DecodeByMimeType (contTyp, resObj)
  , DecodeByMimeType (ContentTypes contTyps, resObj)
  ) => DecodeByMimeType (Response (ContentTypes (contTyp ': contTyps)) resObj) where
  decodeByMimeType mt (Response resObj) = decodeByMimeType @(ContentTypes (contTyp ': contTyps), resObj) mt resObj


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


instance {-# OVERLAPPABLE #-} DecodeByMimeType resSpec => ToRawResponse resSpec where
  toRawResponse = decodeByMimeType @resSpec

instance {-# OVERLAPPABLE #-} EncodeByResponseSpec resSpec => FromRawResponse resSpec where
  fromRawResponse = encodeByResponseSpec @resSpec

instance
  ( Typeable resObj
  , Typeable headers
  , HasContentTypes resTyp
  , UnwrapHeadered headers resObj
  , DecodeByMimeType (resTyp, resObj)
  ) => ToRawResponse (resTyp, Headered headers resObj) where
  toRawResponse mt hdResObj = do
    let (rhds, resObj) = unwrapHeadered hdResObj
    rr <- decodeByMimeType @(resTyp, resObj) mt resObj
    return rr { rawHeaders = rawHeaders rr ++ rhds }

instance
  ( Typeable resObj
  , Typeable headers
  , HasContentTypes resTyp
  , UnwrapHeadered headers resObj
  , DecodeByMimeType (resTyp, resObj)
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
