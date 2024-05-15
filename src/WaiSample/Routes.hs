{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WaiSample.Routes where

import qualified Data.Text       as T
import           Data.Typeable   (Typeable)
import           Web.HttpApiData (FromHttpApiData, ToHttpApiData)

import           WaiSample.Types

root :: Route ()
root = pure ()


path :: T.Text -> Route T.Text
path = LiteralPath


-- :id of /for/example/users/:id
decimalPiece :: Route Integer
decimalPiece = ParsedPath


paramPiece :: forall a. (ToHttpApiData a, FromHttpApiData a, Typeable a) => Route a
paramPiece = ParsedPath


showRoute :: Route a -> T.Text
showRoute = ("/" <>) . go
 where
  go :: Route a -> T.Text
  go (LiteralPath p)    = p
  go (FmapPath _f tbl)  = go tbl
  go (PurePath _x)      = ""
  go (ApPath tblF tblA) = go tblF <> go tblA
  go ParsedPath         = ":param" -- TODO: Name the parameter
