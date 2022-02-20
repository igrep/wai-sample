{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WaiSample.Routes where

import           Data.Proxy      (Proxy (Proxy))
import qualified Data.Text       as T
import           Data.Typeable   (Typeable)
import           Web.HttpApiData (FromHttpApiData, ToHttpApiData)

import           WaiSample.Types

root :: RoutingTable ()
root = pure ()


path :: T.Text -> RoutingTable T.Text
path = LiteralPath


-- :id of /for/example/users/:id
decimalPiece :: RoutingTable Integer
decimalPiece = ParsedPath Proxy


paramPiece :: forall a. (ToHttpApiData a, FromHttpApiData a, Typeable a) => RoutingTable a
paramPiece = ParsedPath (Proxy :: Proxy a)

showRoutes' :: RoutingTable a -> T.Text
showRoutes' (LiteralPath p)    = p
showRoutes' (FmapPath _f tbl)  = showRoutes' tbl
showRoutes' (PurePath _x)      = ""
showRoutes' (ApPath tblF tblA) = showRoutes' tblF <> showRoutes' tblA
showRoutes' (ParsedPath _)     = ":param" -- TODO: Name the parameter
