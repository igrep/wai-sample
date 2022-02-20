{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

import           WaiSample.Types.ContentTypes
import           WaiSample.Types.Status

data Response status resObj = Response
  { statusCode :: status
  , bodyObj    :: resObj
  } deriving (Show, Eq)
  -- TODO: Add other header etc.

data RawResponse = RawResponse
  { rawStatusCode :: Maybe HTS.Status
  , rawBody       :: BL.ByteString
  } deriving (Show, Eq)

defaultRawResponse :: BL.ByteString -> RawResponse
defaultRawResponse = RawResponse Nothing

instance (Typeable status, IsStatusCode status, ToRawResponse resTyp resObj) => ToRawResponse resTyp (Response status resObj) where
  toRawResponse mediaType resTyp res = do
    rr <- toRawResponse mediaType resTyp (bodyObj res)
    return $ RawResponse (Just (toStatusCode (statusCode res))) (rawBody rr)

instance (Typeable status, IsStatusCode status, FromRawResponse resTyp resObj) => FromRawResponse resTyp (Response status resObj) where
  -- fromRawResponse :: MediaType -> resTyp -> RawResponse -> IO resObj
  fromRawResponse mediaType resTyp rr = do
    resObj <- fromRawResponse mediaType resTyp rr
    rawSt <- maybe (fail "Unexpected status code") return $ rawStatusCode rr
    status <- maybe (fail "Unexpected status code") return $ fromStatusCode rawSt
    return $ Response status resObj


data SomeResponse resTyp where
  SomeResponse :: (ToRawResponse resTyp resObj, FromRawResponse resTyp resObj) => resObj -> SomeResponse resTyp

class (HasStatusCode resTyp, HasContentTypes resTyp, Typeable resObj) => ToRawResponse resTyp resObj where
  toRawResponse :: MediaType -> resTyp -> resObj -> IO RawResponse

class (HasStatusCode resTyp, HasContentTypes resTyp, Typeable resObj) => FromRawResponse resTyp resObj where
  fromRawResponse :: MediaType -> resTyp -> RawResponse -> IO resObj

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
