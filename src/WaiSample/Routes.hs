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

showRoutes' :: Route a -> T.Text
showRoutes' (LiteralPath p)    = p
showRoutes' (FmapPath _f tbl)  = showRoutes' tbl
showRoutes' (PurePath _x)      = ""
showRoutes' (ApPath tblF tblA) = showRoutes' tblF <> showRoutes' tblA
showRoutes' ParsedPath         = ":param" -- TODO: Name the parameter
