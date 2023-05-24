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
import           Data.Kind                        (Type)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Network.HTTP.Media               (MediaType)
import           Web.FormUrlEncoded               (FromForm, ToForm,
                                                   urlDecodeAsForm,
                                                   urlEncodeAsForm)

import           Network.HTTP.Media.RenderHeader  (renderHeader)
import qualified Network.HTTP.Types               as HT

import           WaiSample.Types.ContentTypes
import           WaiSample.Types.Response.Headers
import           WaiSample.Types.Status

newtype Response (resTyp :: Type) resObj = Response
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

instance ResponseSpec (resTyp, resObj) where
  type ResponseType (resTyp, resObj) = resTyp
  type ResponseObject (resTyp, resObj) = resObj

instance ResponseSpec (Response resTyp resObj) where
  type ResponseType (Response resTyp resObj) = resTyp
  type ResponseObject (Response resTyp resObj) = Response resTyp resObj

class ResponseSpec resSpec => DecodeByMimeType resSpec where
  decodeByMimeType
    :: MediaType {- ^ Media type determined by 'matchAccept' with client's Accept header -}
    -> ResponseObject resSpec {- ^ Response Object: a value returned by the 'Handler' function. -}
    -> IO RawResponse

class ResponseSpec resSpec => EncodeByMimeType resSpec where
  encodeByMimeType
    :: MediaType {- ^ Media type returned by the server -}
    -> RawResponse {- ^ Response returned by the server whose body's type is unknown. -}
    -> IO (ResponseObject resSpec)


instance ToJSON resObj => DecodeByMimeType (Json, resObj) where
  decodeByMimeType mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . Json.encode

instance ToJSON resObj => DecodeByMimeType (Response Json resObj) where
  decodeByMimeType mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . Json.encode . responseObject

instance FromJSON resObj => EncodeByMimeType (Json, resObj) where
  encodeByMimeType _ = either fail return . Json.eitherDecode' . rawBody

instance FromJSON resObj => EncodeByMimeType (Response Json resObj) where
  encodeByMimeType _ = either fail (return . Response) . Json.eitherDecode' . rawBody


instance ToForm resObj => DecodeByMimeType (FormUrlEncoded, resObj) where
  decodeByMimeType mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . urlEncodeAsForm

instance ToForm resObj => DecodeByMimeType (Response FormUrlEncoded resObj) where
  decodeByMimeType mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . urlEncodeAsForm . responseObject

instance FromForm resObj => EncodeByMimeType (FormUrlEncoded, resObj) where
  encodeByMimeType _ = either (fail . T.unpack) return . urlDecodeAsForm . rawBody

instance FromForm resObj => EncodeByMimeType (Response FormUrlEncoded resObj) where
  encodeByMimeType _ = either (fail . T.unpack) (return . Response) . urlDecodeAsForm . rawBody


instance DecodeByMimeType (PlainText, T.Text) where
  decodeByMimeType mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . BL.fromStrict . TE.encodeUtf8

instance DecodeByMimeType (Response PlainText T.Text) where
  decodeByMimeType mt = return . defaultRawResponse [(HT.hContentType, renderHeader mt)] . BL.fromStrict . TE.encodeUtf8 . responseObject

instance EncodeByMimeType (PlainText, T.Text) where
  encodeByMimeType _ = return . TE.decodeUtf8 . BL.toStrict . rawBody

instance EncodeByMimeType (Response PlainText T.Text) where
  encodeByMimeType _ = return . Response . TE.decodeUtf8 . BL.toStrict . rawBody


instance {-# OVERLAPPING #-}
  ( HasContentTypes contTyp
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
  ( HasContentTypes contTyp
  , HasContentTypes (ContentTypes contTyps)
  , DecodeByMimeType (contTyp, resObj)
  , DecodeByMimeType (ContentTypes contTyps, resObj)
  ) => DecodeByMimeType (ContentTypes (contTyp ': contTyps), resObj) where
  decodeByMimeType mt resObj =
    if matchContentType @contTyp mt
      then decodeByMimeType @(contTyp, resObj) mt resObj
      else decodeByMimeType @(ContentTypes contTyps, resObj) mt resObj

instance
  ( HasContentTypes (ContentTypes contTyps)
  , DecodeByMimeType (ContentTypes contTyps, resObj)
  ) => DecodeByMimeType (Response (ContentTypes contTyps) resObj) where
  decodeByMimeType mt (Response resObj) = decodeByMimeType @(ContentTypes contTyps, resObj) mt resObj


instance {-# OVERLAPPING #-}
  ( HasContentTypes contTyp
  , EncodeByMimeType (contTyp, resObj)
  ) => EncodeByMimeType (ContentTypes '[contTyp], resObj) where
  encodeByMimeType mt rr =
    if matchContentType @contTyp mt
      then encodeByMimeType @(contTyp, resObj) mt rr
      else fail "No content type matched"

instance
  ( HasContentTypes contTyp
  , HasContentTypes (ContentTypes contTyps)
  , EncodeByMimeType (contTyp, resObj)
  , EncodeByMimeType (ContentTypes contTyps, resObj)
  ) => EncodeByMimeType (ContentTypes (contTyp ': contTyps), resObj) where
  encodeByMimeType mt rr =
    if matchContentType @contTyp mt
      then encodeByMimeType @(contTyp, resObj) mt rr
      else encodeByMimeType @(ContentTypes contTyps, resObj) mt rr


instance
  ( HasContentTypes (ContentTypes contTyps)
  , EncodeByMimeType (ContentTypes contTyps, resObj)
  ) => EncodeByMimeType (Response (ContentTypes contTyps) resObj) where
  encodeByMimeType mt rr = Response <$> encodeByMimeType @(ContentTypes contTyps, resObj) mt rr


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

instance {-# OVERLAPPABLE #-} EncodeByMimeType resSpec => FromRawResponse resSpec where
  fromRawResponse = encodeByMimeType @resSpec

instance
  ( UnwrapHeadered headers resObj
  , DecodeByMimeType (resTyp, resObj)
  ) => ToRawResponse (resTyp, Headered headers resObj) where
  toRawResponse mt hdResObj = do
    let (rhds, resObj) = unwrapHeadered hdResObj
    rr <- decodeByMimeType @(resTyp, resObj) mt resObj
    return rr { rawHeaders = rawHeaders rr ++ rhds }

instance
  ( UnwrapHeadered headers resObj
  , DecodeByMimeType (resTyp, resObj)
  ) => ToRawResponse (Response resTyp (Headered headers resObj)) where
  toRawResponse mt (Response hdResObj) = toRawResponse @(resTyp, Headered headers resObj) mt hdResObj


instance
  ( TryWrappingWithHeaders headers resObj
  , EncodeByMimeType (resTyp, resObj)
  ) => FromRawResponse (resTyp, Headered headers resObj) where
  fromRawResponse mt rr = do
    resObj <- encodeByMimeType @(resTyp, resObj) mt rr
    either fail return $ tryWrappingWithHeaders (rawHeaders rr) resObj

instance
  ( TryWrappingWithHeaders headers resObj
  , EncodeByMimeType (resTyp, resObj)
  ) => FromRawResponse (Response resTyp (Headered headers resObj)) where
  fromRawResponse mt rr = Response <$> fromRawResponse @(resTyp, Headered headers resObj) mt rr
