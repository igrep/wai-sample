{-# LANGUAGE DeriveLift                #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}

module WaiSample.Types.ContentTypes where

import qualified Data.Foldable              as F
import           Data.List.NonEmpty         (NonEmpty ((:|)))
import qualified Data.List.NonEmpty         as NE
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Typeable              (Typeable)
import           Language.Haskell.TH.Syntax (Lift)
import           Network.HTTP.Media         (MediaType, (//), (/:))

import           WaiSample.Types.Status


data SomeContentType = forall contTyp. HasContentTypes contTyp => SomeContentType (Proxy contTyp)


class (Typeable contTyp, Lift contTyp) => HasContentTypes contTyp where
  contentTypes :: Proxy contTyp -> NE.NonEmpty MediaType

  matchContentType :: MediaType -> Proxy contTyp -> Maybe SomeContentType
  matchContentType mt contTypP =
    if mt == NE.head (contentTypes contTypP)
      then Just $ SomeContentType contTypP
      else Nothing


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


-- TODO: 次回実装する
data ContentTypes (contTyps :: [*])


-- | The "Choose Content-Type" type
-- FIXME: 次回消す
data contTyp1 |&| contTyp2 = contTyp1 :|&| contTyp2 deriving Lift

instance HasStatusCode (contTyp1 |&| contTyp2)
-- type JsonAndText = Json |&| Text
-- Json |&| Text |&| FormUrlEncoded
-- JsonAndText |&| FormUrlEncoded

instance (HasContentTypes contTyp1, HasContentTypes contTyp2) => HasContentTypes (contTyp1 |&| contTyp2) where
  contentTypes _ = contentTypes (Proxy :: Proxy contTyp1) <> contentTypes (Proxy :: Proxy contTyp2)
  matchContentType mt contTypP =
    -- FIXME: 次回直す
    let mts = contentTypes contTypP
     in case F.find (== mt) mts of
            Just foundMt -> matchContentType mt foundMt resObj
            Nothing      -> matchContentType mt (NE.head mts) resObj
