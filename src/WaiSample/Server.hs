{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module WaiSample.Server where


import           Control.Error.Util               (hush)
import           Control.Exception                (bracket_)
import           Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.Attoparsec.Text             as AT
import           Data.ByteString.Lazy             (fromStrict)
import           Data.CaseInsensitive             (original)
import           Data.Data                        (Proxy)
import qualified Data.List                        as L
import           Data.List.NonEmpty               (NonEmpty ((:|)))
import qualified Data.List.NonEmpty               as NE
import           Data.Maybe                       (fromMaybe, listToMaybe,
                                                   mapMaybe)
import qualified Data.Text                        as T
import           Network.HTTP.Media               (matchAccept)
import           Network.HTTP.Types.Header        (HeaderName, hContentType)
import qualified Network.HTTP.Types.Status        as HTS
import           Network.Wai                      (Application,
                                                   Request (requestHeaders, requestMethod),
                                                   pathInfo, responseLBS)
import qualified Network.Wai                      as Wai
import           Network.Wai.Handler.Warp         (runEnv)
import           System.IO                        (hPutStrLn, stderr)
import           Web.HttpApiData                  (parseHeader, parseUrlPiece)

import qualified Data.ByteString.Lazy.Char8       as BSL
import           WaiSample
import           WaiSample.Internal               (defaultStatusCodeOf)


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
runHandler (Handler (_ :: Proxy resSpec) _name method tbl (_opts :: EndpointOptions h) respond) req =
  act <$> runRoutingTable tbl req
 where
  act x =
    if method == requestMethod req
      then do
        let mMime = matchAccept (contentTypes @(ResponseType resSpec)) acceptHeader
        case mMime of
            Just mime -> do
              let return422 msg =
                    return $ responseLBS
                      HTS.status422
                      [(hContentType, "text/plain;charset=UTF-8")]
                      ("422 Unprocessable Entity: " <> msg)

                  hdCodec :: RequestHeadersCodec h
                  hdCodec = requestHeadersCodec

              case parseRequestHeaders hdCodec req of
                  Right reqHdObj -> do
                    resObj <- respond x (RequestInfo reqHdObj)
                    rawRes <- toRawResponse @resSpec mime resObj
                    let mst = rawStatusCode rawRes
                        stC =
                          case mst of
                              NonDefaultStatus st -> st
                              DefaultStatus       -> defaultStatusCodeOf method
                    return . responseLBS stC (rawHeaders rawRes) $ rawBody rawRes
                  Left (NoHeaderError (name :| others)) ->
                    if null others
                      then return422 $ "request header \"" <> fromStrict (original name) <> "\" is not specified."
                      else return422 $ "Missing request header (one of " <> BSL.pack (show (name : others)) <> ")"
                  Left EmptyRequestHeaderError ->
                    case hdCodec of
                      EmptyRequestHeader -> do
                        -- TODO: Refactor
                        resObj <- respond x (RequestInfo (error "Referring to a void request header value"))
                        rawRes <- toRawResponse @resSpec mime resObj
                        let mst = rawStatusCode rawRes
                            stC =
                              case mst of
                                  NonDefaultStatus st -> st
                                  DefaultStatus       -> defaultStatusCodeOf method
                        return . responseLBS stC (rawHeaders rawRes) $ rawBody rawRes
                      _ -> do
                        -- FIXME!
                        -- TODO: use logger
                        hPutStrLn stderr "[BUG] A product type containing a Void is not currently supported."
                        return422 "Invalid request headers"
                  Left (UnprocessableValueError name) ->
                    return422 $ "request header \"" <> fromStrict (original name) <> "\" is invalid."
            Nothing ->
              return $ responseLBS HTS.status406 [(hContentType, "text/plain;charset=UTF-8")] "406 Not Acceptable."
      else return $ responseLBS HTS.status405 [(hContentType, "text/plain;charset=UTF-8")] "405 Method not allowed."

  acceptHeader = fromMaybe "*/*" . L.lookup "Accept" $ requestHeaders req


parserFromRoutingTable :: Route a -> AT.Parser a
parserFromRoutingTable (LiteralPath p) = AT.string p
parserFromRoutingTable (FmapPath f tbl) = f <$> parserFromRoutingTable tbl
parserFromRoutingTable (PurePath x) = pure x
parserFromRoutingTable (ApPath tblF tblA) = parserFromRoutingTable tblF <*> parserFromRoutingTable tblA
parserFromRoutingTable ParsedPath = parseUrlPiece


runRoutingTable :: Route a -> Request -> Maybe a
runRoutingTable tbl =
  hush . AT.parseOnly (parserFromRoutingTable tbl <* AT.endOfInput) . T.intercalate "/" . pathInfo


parseRequestHeaders :: RequestHeadersCodec h -> Request -> Either RequestHeaderError h
parseRequestHeaders rhp req = run rhp []
 where
  run :: forall h1. RequestHeadersCodec h1 -> [HeaderName] -> Either RequestHeaderError h1
  run (RequestHeader name) notFounds = do
    hdv <-
      case lookup name hds of
        Nothing    -> Left . NoHeaderError $ name :| notFounds
        Just found -> return found
    either (const (Left $ UnprocessableValueError name)) return $
      parseOnly parseHeader hdv
  -- TODO: Test with a record including Void. For example:
  --       (Int, Void)
  --       (Void, Int)
  --       newtype NewVoid = NewVoid Void
  --       Either Void Int
  --       Either Int Void
  run EmptyRequestHeader  notFounds          =
    case notFounds of
      []        -> Left EmptyRequestHeaderError
      n : names -> Left $ NoHeaderError (n :| names)
  run (FmapRequestHeader f rhpA) notFounds   = f <$> run rhpA notFounds
  run (PureRequestHeader x)  _notFounds      = pure x
  run (ApRequestHeader rhpF rhpA) notFounds  = run rhpF notFounds          <*> run rhpA notFounds
  run (AltRequestHeader rhpA rhpB) notFounds =
    case run rhpA notFounds of
      Right a                                -> pure a
      Left (NoHeaderError names)             -> run rhpB $ notFounds ++ NE.toList names
      Left EmptyRequestHeaderError           -> run rhpB notFounds
      Left e@(UnprocessableValueError _name) -> Left e
  hds = requestHeaders req


data RequestHeaderError =
    NoHeaderError (NE.NonEmpty HeaderName)
  | EmptyRequestHeaderError
  | UnprocessableValueError HeaderName
  deriving (Eq, Show)
