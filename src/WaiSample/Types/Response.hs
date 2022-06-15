{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module WaiSample.Types.Response where

import           Data.Aeson                   (FromJSON, ToJSON)
import qualified Data.Aeson                   as Json
import qualified Data.ByteString.Lazy.Char8   as BL
import           Data.Proxy                   (Proxy (Proxy))
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Data.Typeable                (Typeable)
import           Network.HTTP.Media           (MediaType)
import qualified Network.HTTP.Types.Status    as HTS
import           Web.FormUrlEncoded           (FromForm, ToForm,
                                               urlDecodeAsForm, urlEncodeAsForm)

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


class (HasStatusCode resTyp, HasContentTypes resTyp, Typeable resObj) => ToRawResponse resTyp resObj where
  toRawResponse :: MediaType -> Proxy resTyp -> resObj -> IO RawResponse

class (HasStatusCode resTyp, HasContentTypes resTyp, Typeable resObj) => FromRawResponse resTyp resObj where
  fromRawResponse :: MediaType -> Proxy resTyp -> RawResponse -> IO resObj


instance (ToJSON resObj, Typeable resObj) => ToRawResponse Json resObj where
  toRawResponse _ _ = return . defaultRawResponse . Json.encode

instance (FromJSON resObj, Typeable resObj) => FromRawResponse Json resObj where
  fromRawResponse _ _ = either fail return . Json.eitherDecode' . rawBody


instance (ToForm resObj, Typeable resObj) => ToRawResponse FormUrlEncoded resObj where
  toRawResponse _ _ = return . defaultRawResponse . urlEncodeAsForm

instance (FromForm resObj, Typeable resObj) => FromRawResponse FormUrlEncoded resObj where
  fromRawResponse _ _ = either (fail . T.unpack) return . urlDecodeAsForm . rawBody


instance ToRawResponse PlainText T.Text where
  toRawResponse _ _ = return . defaultRawResponse . BL.fromStrict . TE.encodeUtf8

instance FromRawResponse PlainText T.Text where
  fromRawResponse _ _ = return . TE.decodeUtf8 . BL.toStrict . rawBody


instance ToRawResponse contTyp resObj => ToRawResponse (ContentTypes '[contTyp]) resObj where
  toRawResponse mt _ resObj = toRawResponse mt (Proxy :: Proxy contTyp) resObj

instance
  ( ToRawResponse contTyp resObj
  , ToRawResponse (ContentTypes contTyps) resObj
  ) => ToRawResponse (ContentTypes (contTyp ': contTyps)) resObj where
  toRawResponse mt _ resObj =
    case matchContentType mt (Proxy :: Proxy contTyp) of
        Just _contTypP -> toRawResponse mt (Proxy :: Proxy contTyp) resObj
        Nothing -> toRawResponse mt (Proxy :: Proxy (ContentTypes contTyps)) resObj

instance FromRawResponse contTyp resObj => FromRawResponse (ContentTypes '[contTyp]) resObj where
  fromRawResponse mt _ rr = fromRawResponse mt (Proxy :: Proxy contTyp) rr

instance
  ( FromRawResponse contTyp resObj
  , FromRawResponse (ContentTypes contTyps) resObj
  ) => FromRawResponse (ContentTypes (contTyp ': contTyps)) resObj where
  fromRawResponse mt _ rr =
    case matchContentType mt (Proxy :: Proxy contTyp) of
        Just _contTypP -> fromRawResponse mt (Proxy :: Proxy contTyp) rr
        Nothing -> fromRawResponse mt (Proxy :: Proxy (ContentTypes contTyps)) rr
