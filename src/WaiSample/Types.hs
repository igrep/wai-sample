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
  , module WaiSample.Types.Response
  , module WaiSample.Types.Response.Sum
  , module WaiSample.Types.Status
  ) where

import           Data.Proxy                   (Proxy)
import qualified Data.Text                    as T
import           Data.Typeable                (Typeable)
import           Language.Haskell.TH.Syntax   (Lift)
import           Network.HTTP.Types.Method    (Method)
import           Web.HttpApiData              (FromHttpApiData, ToHttpApiData)

import           Control.DeepSeq              (NFData, rnf)
import           WaiSample.Types.ContentTypes
import           WaiSample.Types.Response
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

-- WARNING: This instance is only for using with the shouldNotTypecheck
--          function in InvalidRouteSpec.hs.
instance NFData a => NFData (Route a) where
  rnf (LiteralPath a) = rnf a

  -- NOTE: Impossible to evaluate all of the fields!
  rnf (FmapPath a b)  = seq a (seq b ())

  rnf (PurePath a)    = rnf a

  -- NOTE: Impossible to evaluate all of the fields!
  rnf (ApPath a b)    = seq a (seq b ())

  rnf ParsedPath      = ()

data Handler where
  Handler
    ::
      ( ToRawResponse resSpec
      , FromRawResponse resSpec
      , Typeable resSpec
      , Typeable (ResponseObject resSpec)
      , HasStatusCode (ResponseType resSpec)
      , HasContentTypes (ResponseType resSpec)
      , NFData a
      )
    => Proxy resSpec -> String -> Method -> Route a -> (a -> IO (ResponseObject resSpec)) -> Handler

-- WARNING: This instance is only for using with the shouldNotTypecheck
--          function in InvalidRouteSpec.hs.
instance NFData Handler where
  rnf (Handler a b c d e) = rnf a <> rnf b <> rnf c <> rnf d <> rnf e


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
  , ToRawResponse (resTyp, resObj)
  ) => ToRawResponse (WithStatus status resTyp, resObj) where
  toRawResponse mediaType res = do
    rr <- toRawResponse @(resTyp, resObj) mediaType res
    return $ RawResponse (NonDefaultStatus (toUntypedStatusCode @status)) (rawBody rr)

instance
  ( Typeable status
  , Lift status
  , IsStatusCode status
  , HasContentTypes resTyp
  , Typeable resObj
  , ToRawResponse (resTyp, resObj)
  ) => ToRawResponse (Response (WithStatus status resTyp) resObj) where
  toRawResponse mediaType (Response resObj) =
    toRawResponse @(WithStatus status resTyp, resObj) mediaType resObj


instance
  ( Typeable status
  , Lift status
  , IsStatusCode status
  , HasContentTypes resTyp
  , Typeable resObj
  , FromRawResponse (resTyp, resObj)
  ) => FromRawResponse (WithStatus status resTyp, resObj) where
  fromRawResponse mediaType rr = do
    resObj <- fromRawResponse @(resTyp, resObj) mediaType rr
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
  , FromRawResponse (resTyp, resObj)
  ) => FromRawResponse (Response (WithStatus status resTyp) resObj) where
  fromRawResponse mediaType rr =
    Response <$> fromRawResponse @(WithStatus status resTyp, resObj) mediaType rr
