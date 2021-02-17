{-# LANGUAGE TemplateHaskell #-}
module WaiSample.Client
  ( declareClient
  , Backend
  ) where

import           Control.Monad        (foldM)
import qualified Data.ByteString.Lazy as BL
import           Data.Char            (toUpper)
import           Language.Haskell.TH  (Dec, DecQ, DecsQ, mkName, sigD)
import           WaiSample


declareClient :: String -> [Handler] -> DecsQ
declareClient prefix = foldM f []
 where
  f :: [Dec] -> Handler -> DecsQ
  f decs handler = do
    sigDAndFunD <- declareEndpointFunction handler
    return $ sigDAndFunD ++ decs

  declareEndpointFunction :: Handler -> DecsQ
  declareEndpointFunction (Handler handlerName tbl _action toFromResponseBody) = do
    funName <- mkName $ makeUpName handlerName
    [d|
      $(sigD funName [t| Backend |])
      |]

  makeUpName :: String -> String
  makeUpName handlerName =
    if null prefix
      then prefix ++ toUpperFirst handlerName
      else handlerName

  toUpperFirst :: String -> String
  toUpperFirst (first : left) = toUpper first : left
  toUpperFirst _              = error "toUpperFirst: Empty handler name!"


type Backend = String -> IO BL.ByteString
