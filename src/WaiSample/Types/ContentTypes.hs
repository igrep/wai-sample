{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveLift                #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeOperators             #-}

module WaiSample.Types.ContentTypes where

import           Control.Applicative ((<|>))
import           Data.List.NonEmpty         (NonEmpty ((:|)))
import qualified Data.List.NonEmpty         as NE
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Typeable              (Typeable)
import           Language.Haskell.TH.Syntax (Lift)
import           Network.HTTP.Media         (MediaType, (//), (/:))

import           WaiSample.Types.Status


data SomeContentType = forall contTyp. HasContentTypes contTyp => SomeContentType (Proxy contTyp)


-- TODO: 複数形の HasContentTypes と 単数の HasContentType で別の型クラスにする
class (Typeable contTyp, Lift contTyp) => HasContentTypes contTyp where
  contentTypes :: Proxy contTyp -> [MediaType]

  matchContentType :: MediaType -> Proxy contTyp -> Maybe SomeContentType
  matchContentType mt contTypP =
    case contentTypes contTypP of
        (first : _)
          | mt == first -> Just $ SomeContentType contTypP
        _ -> Nothing


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


data ContentTypes (contTyps :: [*])

deriving instance Lift (ContentTypes contTyps)

instance HasStatusCode (ContentTypes contTyps)

instance HasContentTypes (ContentTypes '[]) where
  contentTypes _ = []
  matchContentType _ _ =
    Nothing

instance
  ( Typeable contTyps
  , HasContentTypes contTyp
  , HasContentTypes (ContentTypes contTyps)
  ) => HasContentTypes (ContentTypes (contTyp ': contTyps)) where
  contentTypes _ = contentTypes (Proxy :: Proxy contTyp) <> contentTypes (Proxy :: Proxy (ContentTypes contTyps))
  matchContentType mt _contTypP =
    matchContentType mt (Proxy :: Proxy contTyp)
      <|> matchContentType mt (Proxy :: Proxy (ContentTypes contTyps))
