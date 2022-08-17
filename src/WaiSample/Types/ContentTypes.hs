{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveLift                #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}

module WaiSample.Types.ContentTypes where

import           Control.Applicative        ((<|>))
import           Data.Proxy                 (Proxy (Proxy))
import           Language.Haskell.TH.Syntax (Lift)
import           Network.HTTP.Media         (MediaType, (//), (/:))

import           WaiSample.Types.Status


data SomeContentType = forall contTyp. HasContentTypes contTyp => SomeContentType (Proxy contTyp)


-- TODO: 複数形のHasContentTypesと単数のHasContentTypeで別の型クラスにする
--       複数形のHasContentTypesにはmatchContentTypeが必要ないっぽい
class Lift contTyp => HasContentTypes contTyp where
  contentTypes :: [MediaType]

  -- TODO: SomeContentTypeがどのcontTypについてのProxyかは引数により自明なので、
  --       Maybe SomeContentTypeではなくBoolを返す
  matchContentType :: MediaType -> Maybe SomeContentType
  matchContentType mt =
    case contentTypes @contTyp of
        (first : _)
          | mt == first -> Just $ SomeContentType (Proxy :: Proxy contTyp)
        _ -> Nothing


-- TODO: Remove value constructors in the types in this module.
data Json = Json deriving Lift

instance HasStatusCode Json

instance HasContentTypes Json where
  contentTypes = ["application" // "json"]


data FormUrlEncoded = FormUrlEncoded deriving Lift

instance HasStatusCode FormUrlEncoded

instance HasContentTypes FormUrlEncoded where
  contentTypes = ["application" // "x-www-form-urlencoded"]


data PlainText = PlainText deriving Lift

instance HasStatusCode PlainText

instance HasContentTypes PlainText where
  contentTypes = ["text" // "plain" /: ("charset", "UTF-8")]


data ContentTypes (contTyps :: [*])

deriving instance Lift (ContentTypes contTyps)

instance HasStatusCode (ContentTypes contTyps)

instance HasContentTypes (ContentTypes '[]) where
  contentTypes = []
  matchContentType _ = Nothing

instance
  ( HasContentTypes contTyp
  , HasContentTypes (ContentTypes contTyps)
  ) => HasContentTypes (ContentTypes (contTyp ': contTyps)) where
  contentTypes = contentTypes @contTyp <> contentTypes @(ContentTypes contTyps)
  matchContentType mt =
    matchContentType @contTyp mt
      <|> matchContentType @(ContentTypes contTyps) mt
