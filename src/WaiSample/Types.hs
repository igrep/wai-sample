{-# LANGUAGE DeriveLift                #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}

module WaiSample.Types
  ( RoutingTable (..)
  , Handler (..)
  , ChooseResponseType (..)
  , WithStatus (..)
  , module WaiSample.Types.ContentTypes
  , module WaiSample.Types.Response
  , module WaiSample.Types.Status
  ) where

import qualified Data.Foldable                as F
import           Data.Proxy                   (Proxy)
import qualified Data.Text                    as T
import           Data.Typeable                (Typeable)
import           Language.Haskell.TH.Syntax   (Lift)
import           Network.HTTP.Types.Method    (Method)
import           Web.HttpApiData              (FromHttpApiData, ToHttpApiData)

import           WaiSample.Types.ContentTypes
import           WaiSample.Types.Response
import           WaiSample.Types.Status


-- TODO: Rename into Route?
data RoutingTable a where
  LiteralPath :: T.Text -> RoutingTable T.Text
  -- | '<$>'
  FmapPath :: (a -> b) -> RoutingTable a -> RoutingTable b
  PurePath :: a -> RoutingTable a
  -- | '<*>'
  ApPath :: RoutingTable (a -> b) -> RoutingTable a -> RoutingTable b
  ParsedPath :: (ToHttpApiData a, FromHttpApiData a, Typeable a) => Proxy a -> RoutingTable a

instance Functor RoutingTable where
  fmap = FmapPath

instance Applicative RoutingTable where
  pure = PurePath
  (<*>) = ApPath

data Handler where
  Handler
    ::
     ( Typeable a
     , HasStatusCode resTyp -- TODO: Remove?
     , HasContentTypes resTyp -- TODO: Remove?
     , ToRawResponse resTyp resObj
     , FromRawResponse resTyp resObj
     )
    => String -> Method -> RoutingTable a -> resTyp -> (a -> IO resObj) -> Handler
    --                                                          ^^^^^^
    --                                                          Text
    --                                                      (Response Status404 Text)

data ChooseResponseType a b = a :<|> b deriving Lift

instance (HasStatusCode a, HasStatusCode b) => HasStatusCode (ChooseResponseType a b) where
  statusCodes (a :<|> b) = statusCodes a ++ statusCodes b

instance (HasContentTypes a, HasContentTypes b) => HasContentTypes (ChooseResponseType a b) where
  contentTypes (a :<|> b) = contentTypes a <> contentTypes b

-- TODO: 実際のstatus codeに応じて結果を変える
instance (ToRawResponse a resObj, ToRawResponse b resObj, Typeable resObj) => ToRawResponse (ChooseResponseType a b) resObj where
  toRawResponse mediaType (a :<|> b) resObj
    | mediaType `F.elem` contentTypes a = toRawResponse mediaType a resObj
    | mediaType `F.elem` contentTypes b = toRawResponse mediaType b resObj
    | otherwise = fail "No suitable media type"

-- TODO: 実際のstatus codeに応じて結果を変える
instance (FromRawResponse a resObj, FromRawResponse b resObj) => FromRawResponse (ChooseResponseType a b) resObj where
  fromRawResponse mediaType (a :<|> b) bs
    | mediaType `F.elem` contentTypes a = fromRawResponse mediaType a bs
    | mediaType `F.elem` contentTypes b = fromRawResponse mediaType b bs
    | otherwise = fail "No suitable media type" -- Perhaps should improve this error message.

data WithStatus status resTyp = WithStatus status resTyp deriving (Eq, Show, Lift)

-- NOTE: Current implementation has a problem that for example `WithStatus 404 (WithStatus 500 PlainText)` ignores 500. But I'll ignore the problem
instance IsStatusCode status => HasStatusCode (WithStatus status resTyp) where
  statusCodes (WithStatus st _resTyp) = [toStatusCode st]

instance (Lift status, HasContentTypes resTyp) => HasContentTypes (WithStatus status resTyp) where
  contentTypes (WithStatus _st resTyp) = contentTypes resTyp


instance (Typeable status, Lift status, IsStatusCode status, ToRawResponse resTyp resObj) => ToRawResponse (WithStatus status resTyp) (Response status resObj) where
  toRawResponse mediaType (WithStatus _st resTyp) res = do
    rr <- toRawResponse mediaType resTyp (bodyObject res)
    return $ RawResponse (Just (toStatusCode (statusCode res))) (rawBody rr)


instance (Typeable status, Lift status, IsStatusCode status, FromRawResponse resTyp resObj) => FromRawResponse (WithStatus status resTyp) (Response status resObj) where
  fromRawResponse mediaType (WithStatus _st resTyp) rr = do
    resObj <- fromRawResponse mediaType resTyp rr
    rawSt <- maybe (fail "Unexpected status code") return $ rawStatusCode rr
    status <- maybe (fail "Unexpected status code") return $ fromStatusCode rawSt
    return $ Response status resObj
