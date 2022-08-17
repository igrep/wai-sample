{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module WaiSample.Server where


import           Control.Error.Util        (hush)
import           Control.Exception         (bracket_)
import qualified Data.Attoparsec.Text      as AT
import qualified Data.List                 as L
import qualified Data.List.NonEmpty        as NE
import           Data.Maybe                (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text                 as T
import           Network.HTTP.Media        (matchAccept, renderHeader)
import           Network.HTTP.Types.Header (hContentType)
import           Network.HTTP.Types.Method (methodPost)
import qualified Network.HTTP.Types.Status as HTS
import           Network.Wai               (Application, Request (requestHeaders, requestMethod),
                                            pathInfo, responseLBS)
import qualified Network.Wai               as Wai
import           Network.Wai.Handler.Warp  (runEnv)
import           Web.HttpApiData           (parseUrlPiece)

import           Data.Data                 (Proxy)
import           WaiSample


sampleApp :: Application
sampleApp = handles sampleRoutes


runSampleApp :: IO ()
runSampleApp = runEnv 8020 sampleApp


handles :: [Handler] -> Application
handles hdls req respond' = bracket_ (return ()) (return ()) $ do
  let foundResponds = listToMaybe $ mapMaybe (`runHandler` req) hdls
  case foundResponds of
      Just respond -> respond' =<< respond
      Nothing      -> respond' handle404
 where
  handle404 = responseLBS HTS.status404 [(hContentType, "text/plain;charset=UTF-8")] "404 Not found."


runHandler :: Handler -> Request -> Maybe (IO Wai.Response)
runHandler (Handler (resSpec :: Proxy resSpec) _name method tbl hdl) req =
  act <$> runRoutingTable tbl req
 where
  act x =
    if method == requestMethod req
      then do
        let mMime = matchAccept (contentTypes @resSpec) acceptHeader
        case mMime of
            Just mime -> do
              resObj <- hdl x
              rawRes <- toRawResponse @resSpec mime resObj
              let mst = rawStatusCode rawRes
                  stC =
                    case mst of
                        NonDefaultStatus st -> st
                        DefaultStatus ->
                          if method == methodPost then HTS.status201 else HTS.status200
              return . responseLBS stC [(hContentType, renderHeader mime)] $ rawBody rawRes
            Nothing ->
              return $ responseLBS HTS.status406 [(hContentType, "text/plain;charset=UTF-8")] "406 Not Acceptable"
      else return $ responseLBS HTS.status405 [(hContentType, "text/plain;charset=UTF-8")] "405 Method not allowed."

  acceptHeader = fromMaybe "*/*" . L.lookup "Accept" $ requestHeaders req


parserFromRoutingTable :: RoutingTable a -> AT.Parser a
parserFromRoutingTable (LiteralPath p) = AT.string p
parserFromRoutingTable (FmapPath f tbl) = f <$> parserFromRoutingTable tbl
parserFromRoutingTable (PurePath x) = pure x
parserFromRoutingTable (ApPath tblF tblA) = parserFromRoutingTable tblF <*> parserFromRoutingTable tblA
parserFromRoutingTable (ParsedPath _) = parseUrlPiece


runRoutingTable :: RoutingTable a -> Request -> Maybe a
runRoutingTable tbl =
  hush . AT.parseOnly (parserFromRoutingTable tbl <* AT.endOfInput) . T.intercalate "/" . pathInfo
