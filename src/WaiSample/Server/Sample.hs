module WaiSample.Server.Sample where

import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (runEnv)

import           WaiSample.Sample         (sampleRoutes)
import           WaiSample.Server         (handles)


sampleApp :: Application
sampleApp = handles sampleRoutes


runSampleApp :: IO ()
runSampleApp = runEnv 8020 sampleApp
