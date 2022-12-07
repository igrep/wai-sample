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


class Lift contTyp => HasContentTypes contTyp where
  contentTypes :: [MediaType]

  matchContentType :: MediaType -> Bool
  matchContentType mt =
    case contentTypes @contTyp of
        (first : _) -> mt == first
        _           -> False


data Json deriving Lift

instance HasContentTypes Json where
  contentTypes = ["application" // "json"]


data FormUrlEncoded deriving Lift

instance HasContentTypes FormUrlEncoded where
  contentTypes = ["application" // "x-www-form-urlencoded"]


data PlainText deriving Lift

instance HasContentTypes PlainText where
  contentTypes = ["text" // "plain" /: ("charset", "UTF-8")]


data ContentTypes (contTyps :: [Type])

deriving instance Lift (ContentTypes contTyps)

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
