{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module WaiSample.Client.Sample where

import           WaiSample
import           WaiSample.Client


$(declareClient "sample" sampleRoutes)
