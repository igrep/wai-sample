{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE OverloadedStrings #-}

module WaiSample.Types.ContentTypes where

import           Data.List.NonEmpty         (NonEmpty ((:|)))
import qualified Data.List.NonEmpty         as NE
import           Data.Proxy                 (Proxy)
import           Language.Haskell.TH.Syntax (Lift)
import           Network.HTTP.Media         (MediaType, (//), (/:))

import           WaiSample.Types.Status


class Lift resTyp => HasContentTypes resTyp where
  contentTypes :: Proxy resTyp -> NE.NonEmpty MediaType

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
