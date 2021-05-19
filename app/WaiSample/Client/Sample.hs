{-# LANGUAGE TemplateHaskell #-}

module WaiSample.Client.Sample where

import           WaiSample
import           WaiSample.Client


$(declareClient "sample" routes)
