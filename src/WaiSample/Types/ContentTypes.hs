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

import           Language.Haskell.TH.Syntax (Lift)
import           Network.HTTP.Media         (MediaType, (//), (/:))

import           Data.Kind                  (Type)
import           WaiSample.Types.Status


class Lift contTyp => HasContentTypes contTyp where
  contentTypes :: [MediaType]

  matchContentType :: MediaType -> Bool
  matchContentType mt =
    case contentTypes @contTyp of
        (first : _) -> mt == first
        _           -> False


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


data ContentTypes (contTyps :: [Type])

deriving instance Lift (ContentTypes contTyps)

instance HasStatusCode (ContentTypes contTyps)

instance {-# OVERLAPPING #-}
  HasContentTypes contTyp => HasContentTypes (ContentTypes '[contTyp]) where
  contentTypes = contentTypes @contTyp
  matchContentType = matchContentType @contTyp

instance
  ( HasContentTypes contTyp
  , HasContentTypes (ContentTypes contTyps)
  ) => HasContentTypes (ContentTypes (contTyp ': contTyps)) where
  contentTypes = contentTypes @contTyp <> contentTypes @(ContentTypes contTyps)
  matchContentType mt =
    matchContentType @contTyp mt
      || matchContentType @(ContentTypes contTyps) mt
