{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

module WaiSample.InvalidRouteSpec
  ( spec
  ) where


import           Control.DeepSeq   (NFData, force)
import           Control.Exception (TypeError (TypeError), evaluate, throwIO,
                                    try)
import qualified Data.Text         as T
import           Test.Syd          (Assertion (ExpectationFailed), Spec,
                                    describe, it)

import           Control.Monad     (when)
import           Data.List         (isInfixOf, isSuffixOf)
import           WaiSample         (Sum, decimalPiece, get, path)


spec :: Spec
spec = describe "WaiSample.Client.Sample.get" $
  it "won't type-check if the resSpec is an empty Sum." $ do
    shouldNotTypecheck $
      get @(Sum '[]) "emptySome"
        (path "customer/" *> decimalPiece)
        (\_ -> return ("text" :: T.Text))


shouldNotTypecheck :: NFData a => (() ~ () => a) -> IO ()
shouldNotTypecheck a = do
  result <- try (evaluate $ force a)
  case result of
    Right _ -> throwIO $ ExpectationFailed "Expected expression to not compile but it did compile"
    Left e@(TypeError msg) ->
      if "(deferred type error)" `isSuffixOf` msg
        then
          when (isInfixOf "No instance for" msg && isInfixOf "NFData" msg) $
            throwIO $ ExpectationFailed $ "Make sure the expression has an NFData instance! Full error:\n" ++ msg
        else
          throwIO e
