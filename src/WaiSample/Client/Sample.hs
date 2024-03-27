{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module WaiSample.Client.Sample where

import           WaiSample.Client
import           WaiSample.Sample


$(declareClient "sample" sampleRoutes)
