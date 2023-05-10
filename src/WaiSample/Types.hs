{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module WaiSample.Types
  ( Route (..)
  , Handler (..)
  , WithStatus (..)
  , module WaiSample.Types.ContentTypes
  , module WaiSample.Types.Response.Headers
  , module WaiSample.Types.Response
  , module WaiSample.Types.Response.Sum
  , module WaiSample.Types.Status
  ) where

import           Data.Proxy                       (Proxy)
import qualified Data.Text                        as T
import           Data.Typeable                    (Typeable)
import           Language.Haskell.TH.Syntax       (Lift)
import           Network.HTTP.Types.Method        (Method)
import           Web.HttpApiData                  (FromHttpApiData,
                                                   ToHttpApiData)

import           WaiSample.Types.ContentTypes
import           WaiSample.Types.Response
import           WaiSample.Types.Response.Headers
import           WaiSample.Types.Response.Sum
import           WaiSample.Types.Status


data Route a where
  LiteralPath :: T.Text -> Route T.Text
  -- | '<$>'
  FmapPath :: (a -> b) -> Route a -> Route b
  PurePath :: a -> Route a
  -- | '<*>'
  ApPath :: Route (a -> b) -> Route a -> Route b
  ParsedPath :: (ToHttpApiData a, FromHttpApiData a, Typeable a) => Route a

instance Functor Route where
  fmap = FmapPath

instance Applicative Route where
  pure = PurePath
  (<*>) = ApPath

data Handler where
  Handler
    ::
      ( ToRawResponse resSpec
      , FromRawResponse resSpec
      , Typeable resSpec
      , Typeable (ResponseObject resSpec)
      , HasStatusCode (ResponseType resSpec)
      , HasContentTypes (ResponseType resSpec)
      )
    => Proxy resSpec -> String -> Method -> Route a -> (a -> IO (ResponseObject resSpec)) -> Handler


data WithStatus status resTyp = WithStatus status resTyp deriving (Eq, Show, Lift)

-- NOTE: Current implementation has a problem that for example `WithStatus 404 (WithStatus 500 PlainText)` ignores 500. But I'll ignore the problem
instance IsStatusCode status => HasStatusCode (WithStatus status resTyp) where
  statusCodes = [NonDefaultStatus $ toUntypedStatusCode @status]

instance (Lift status, HasContentTypes resTyp) => HasContentTypes (WithStatus status resTyp) where
  contentTypes = contentTypes @resTyp


instance
  ( Typeable status
  , Lift status
  , IsStatusCode status
  , HasContentTypes resTyp
  , Typeable resObj
  , DecodeByMimeType (resTyp, resObj)
  ) => DecodeByMimeType (WithStatus status resTyp, resObj) where
  decodeByMimeType mediaType res = do
    rr <- decodeByMimeType @(resTyp, resObj) mediaType res
    return $ RawResponse (NonDefaultStatus (toUntypedStatusCode @status)) (rawHeaders rr) (rawBody rr)

instance
  ( Typeable status
  , Lift status
  , IsStatusCode status
  , HasContentTypes resTyp
  , Typeable resObj
  , DecodeByMimeType (resTyp, resObj)
  ) => DecodeByMimeType (Response (WithStatus status resTyp) resObj) where
  decodeByMimeType mediaType (Response resObj) =
    decodeByMimeType @(WithStatus status resTyp, resObj) mediaType resObj


instance
  ( Typeable status
  , Lift status
  , IsStatusCode status
  , HasContentTypes resTyp
  , Typeable resObj
  , EncodeByResponseSpec (resTyp, resObj)
  ) => EncodeByResponseSpec (WithStatus status resTyp, resObj) where
  encodeByResponseSpec mediaType rr = do
    resObj <- encodeByResponseSpec @(resTyp, resObj) mediaType rr
    rawSt <-
      case rawStatusCode rr of
          DefaultStatus       -> fail "Unexpected status code"
          NonDefaultStatus st -> return st
    _ <- maybe (fail "Unexpected status code") return $ fromStatusCode @status rawSt
    return resObj

instance
  ( Typeable status
  , Lift status
  , IsStatusCode status
  , HasContentTypes resTyp
  , Typeable resObj
  , EncodeByResponseSpec (resTyp, resObj)
  ) => EncodeByResponseSpec (Response (WithStatus status resTyp) resObj) where
  encodeByResponseSpec mediaType rr =
    Response <$> encodeByResponseSpec @(WithStatus status resTyp, resObj) mediaType rr
