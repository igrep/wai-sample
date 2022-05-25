{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module WaiSample.Types.ContentTypes where

import           Data.List.NonEmpty         (NonEmpty ((:|)))
import qualified Data.List.NonEmpty         as NE
import           Data.Proxy                 (Proxy (Proxy))
import           Language.Haskell.TH.Syntax (Lift)
import           Network.HTTP.Media         (MediaType, (//), (/:))

import           WaiSample.Types.Status


class Lift contTyp => HasContentTypes contTyp where
  contentTypes :: Proxy contTyp -> NE.NonEmpty MediaType
  matchContentType :: MediaType -> Proxy contTyp -> Maybe (Proxy someContTyp)


-- TODO: Remove value constructors in the types in this module.
data Json = Json deriving Lift

instance HasStatusCode Json

instance HasContentTypes Json where
  contentTypes _ = "application" // "json" :| []


data FormUrlEncoded = FormUrlEncoded deriving Lift

instance HasStatusCode FormUrlEncoded

instance HasContentTypes FormUrlEncoded where
  contentTypes _ = "application" // "x-www-form-urlencoded" :| []


data PlainText = PlainText deriving Lift

instance HasStatusCode PlainText

instance HasContentTypes PlainText where
  contentTypes _ = "text" // "plain" /: ("charset", "UTF-8") :| []


-- | The "Choose Content-Type" type
data contTyp1 |&| contTyp2 = contTyp1 :|&| contTyp2 deriving Lift

instance HasStatusCode (contTyp1 |&| contTyp2)

instance (HasContentTypes contTyp1, HasContentTypes contTyp2) => HasContentTypes (contTyp1 |&| contTyp2) where
  contentTypes _ = contentTypes (Proxy :: Proxy contTyp1) <> contentTypes (Proxy :: Proxy contTyp2)
