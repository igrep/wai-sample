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

import           Data.Aeson                   (FromJSON, ToJSON)
import qualified Data.Aeson                   as Json
import qualified Data.ByteString.Lazy.Char8   as BL
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Data.Typeable                (Typeable)
import           Network.HTTP.Media           (MediaType)
import qualified Network.HTTP.Types.Status    as HTS
import           Web.FormUrlEncoded           (FromForm, ToForm,
                                               urlDecodeAsForm, urlEncodeAsForm)

import           Data.Proxy                   (Proxy (Proxy))
import           WaiSample.Types.ContentTypes
import           WaiSample.Types.Status

data Response status resObj = Response
  { statusCode :: status
  , bodyObject :: resObj
  } deriving (Show, Eq)
  -- TODO: Add other header etc.

data RawResponse = RawResponse
  { rawStatusCode :: Maybe HTS.Status
  , rawBody       :: BL.ByteString
  } deriving (Show, Eq)

defaultRawResponse :: BL.ByteString -> RawResponse
defaultRawResponse = RawResponse Nothing


class ResponseSpec resSpec where
  type ResponseObject resSpec

instance (HasStatusCode resTyp, HasContentTypes resTyp, Typeable resObj) => ResponseSpec (resTyp, resObj) where
  type ResponseObject (resTyp, resObj) = resObj

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
  toRawResponse _ = return . defaultRawResponse . Json.encode

instance (FromJSON resObj, Typeable resObj) => FromRawResponse (Json, resObj) where
  fromRawResponse _ = either fail return . Json.eitherDecode' . rawBody


instance (ToForm resObj, Typeable resObj) => ToRawResponse (FormUrlEncoded, resObj) where
  toRawResponse _ = return . defaultRawResponse . urlEncodeAsForm

instance (FromForm resObj, Typeable resObj) => FromRawResponse (FormUrlEncoded, resObj) where
  fromRawResponse _ = either (fail . T.unpack) return . urlDecodeAsForm . rawBody


instance ToRawResponse (PlainText, T.Text) where
  toRawResponse _ = return . defaultRawResponse . BL.fromStrict . TE.encodeUtf8

instance FromRawResponse (PlainText, T.Text) where
  fromRawResponse _ = return . TE.decodeUtf8 . BL.toStrict . rawBody


instance Typeable resObj => ToRawResponse (ContentTypes '[], resObj) where
  toRawResponse _ _ = fail "Impossible: Empty ContentTypes"

instance
  ( Typeable resObj
  , HasContentTypes contTyp
  , HasContentTypes (ContentTypes contTyps)
  , ToRawResponse (contTyp, resObj)
  , ToRawResponse (ContentTypes contTyps, resObj)
  ) => ToRawResponse (ContentTypes (contTyp ': contTyps), resObj) where
  toRawResponse mt resObj =
    case matchContentType mt (Proxy :: Proxy contTyp) of
        Just (SomeContentType _contTyp) ->
          toRawResponse @(contTyp, resObj) mt resObj
        Nothing ->
          toRawResponse @(ContentTypes contTyps, resObj) mt resObj


instance Typeable resObj => FromRawResponse (ContentTypes '[], resObj) where
  fromRawResponse _ _ = fail "Impossible: Empty ContentTypes"

instance
  ( Typeable resObj
  , HasContentTypes contTyp
  , HasContentTypes (ContentTypes contTyps)
  , FromRawResponse (contTyp, resObj)
  , FromRawResponse (ContentTypes contTyps, resObj)
  ) => FromRawResponse (ContentTypes (contTyp ': contTyps), resObj) where
  fromRawResponse mt rr =
    case matchContentType mt (Proxy :: Proxy contTyp) of
        Just (SomeContentType _contTyp) ->
          fromRawResponse @(contTyp, resObj) mt rr
        Nothing ->
          fromRawResponse @(ContentTypes contTyps, resObj) mt rr
