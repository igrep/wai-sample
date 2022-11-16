{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

module WaiSample.InvalidRouteSpec
  ( spec
  ) where


import           Control.DeepSeq   (NFData (rnf), force)
import           Control.Exception (TypeError (TypeError), evaluate, throwIO,
                                    try)
import qualified Data.Text         as T
import           Test.Syd          (Assertion (ExpectationFailed), Spec,
                                    describe, it)

import           Control.Monad     (when)
import           Data.List         (isInfixOf, isSuffixOf)
import           GHC.Stack         (HasCallStack)
import           WaiSample         (ContentTypes, Handler (Handler), Route (ApPath, FmapPath, LiteralPath, ParsedPath, PurePath),
                                    Sum, decimalPiece, get, path)


newtype NfHandler = NfHandler Handler

rnfRoute :: Route a -> ()
rnfRoute (LiteralPath a) = rnf a
-- NOTE: Impossible to evaluate all of the fields!
rnfRoute (FmapPath a b)  = seq a (seq b ())
rnfRoute (PurePath a)    = rnf a
-- NOTE: Impossible to evaluate all of the fields!
rnfRoute (ApPath a b)    = seq a (seq b ())
rnfRoute ParsedPath      = ()

instance NFData NfHandler where
  rnf (NfHandler (Handler a b c d e)) = rnf a <> rnf b <> rnf c <> rnfRoute d <> rnf e


spec :: HasCallStack => Spec
spec = describe "WaiSample.Client.Sample.get" $ do
  it "won't type-check if the resSpec is an empty Sum." $ do
    shouldNotTypecheck $ NfHandler $
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
          when ("NFData" `isInfixOf` msg) $
            throwIO $ ExpectationFailed $ "Make sure the expression has an NFData instance! Full error:\n" ++ msg
        else
          throwIO e
