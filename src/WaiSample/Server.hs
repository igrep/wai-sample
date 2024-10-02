{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module WaiSample.Server where


import           Control.Error.Util         (failWith, hush)
import           Control.Exception          (bracket_)
import           Control.Monad              (guard)
import           Control.Monad.Except       (ExceptT (..), runExceptT)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Attoparsec.Text       as AT
import           Data.ByteString.Lazy       (fromStrict)
import           Data.CaseInsensitive       (original)
import           Data.Data                  (Proxy)
import qualified Data.List                  as L
import           Data.List.NonEmpty         (NonEmpty ((:|)))
import           Data.Maybe                 (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text                  as T
import           Network.HTTP.Media         (matchAccept)
import           Network.HTTP.Types.Header  (hContentType)
import qualified Network.HTTP.Types.Status  as HTS
import           Network.Wai                (Application,
                                             Request (requestHeaders, requestMethod),
                                             pathInfo, queryString, responseLBS)
import qualified Network.Wai                as Wai
import           Web.HttpApiData            (parseUrlPiece)

import           Data.Bifunctor             (first, second)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           WaiSample
import           WaiSample.Internal         (defaultStatusCodeOf)


handles :: [Handler] -> Application
handles hdls req respond' = bracket_ (return ()) (return ()) $ do
  let foundResponds = listToMaybe $ mapMaybe (`runHandler` req) hdls
  case foundResponds of
      Just respond -> respond' =<< respond
      Nothing      -> respond' handle404
 where
  handle404 = responseLBS HTS.status404 [(hContentType, "text/plain;charset=UTF-8")] "404 Not found."


runHandler :: Handler -> Request -> Maybe (IO Wai.Response)
runHandler (Handler (_ :: Proxy resSpec) _name method tbl (_opts :: EndpointOptions q h) respond) req =
  act <$> runRoutingTable tbl req
 where
  act x = fmap (either respondWithHttpError id) . runExceptT $ do
    failWith (HttpError HTS.status405 "405 Method not allowed.")
      . guard $ method == requestMethod req

    mime <- failWith (HttpError HTS.status406 "406 Not Acceptable.") $
      matchAccept (contentTypes @(ResponseType resSpec)) acceptHeader

    q <- handleFromQueryParamsResult . fromQueryParams . map (second (fromMaybe "")) $ queryString req
    h <- handleFromRequestHeadersResult $ fromRequestHeaders (requestHeaders req)
    resObj <- liftIO $ respond x (RequestInfo q h)
    rawRes <- liftIO $ toRawResponse @resSpec mime resObj
    let mst = rawStatusCode rawRes
        stC =
          case mst of
              NonDefaultStatus st -> st
              DefaultStatus       -> defaultStatusCodeOf method
    return . responseLBS stC (rawHeaders rawRes) $ rawBody rawRes

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


data HttpError =
  HttpError
  { httpErrorStatus :: HTS.Status
  , httpErrorBody   :: BSL.ByteString
  }
  deriving (Eq, Show)


respondWithHttpError :: HttpError -> Wai.Response
respondWithHttpError e =
  responseLBS (httpErrorStatus e) [(hContentType, "text/plain;charset=UTF-8")] (httpErrorBody e)


handleFromQueryParamsResult :: FromQueryParamsResult q -> ExceptT HttpError IO q
handleFromQueryParamsResult = ExceptT . return . first f . unFromQueryParamsResult
 where
  f (NoQueryItemError (name :| others)) =
    if null others
      then HttpError HTS.status422 $ "422 Unprocessable Entity: Missing query parameter \"" <> fromStrict name <> "\""
      else HttpError HTS.status422 $ "422 Unprocessable Entity: Missing query parameter (one of " <> BSL.pack (show (name : others)) <> ")"
  f (UnprocessableQueryValueError name) =
    HttpError HTS.status422 $ "422 Unprocessable Entity: Value of the query parameter \"" <> fromStrict name <> "\" is invalid."


handleFromRequestHeadersResult :: FromRequestHeadersResult q -> ExceptT HttpError IO q
handleFromRequestHeadersResult = ExceptT . return . first f . unFromRequestHeadersResult
 where
  f (NoHeaderError (name :| others)) =
    if null others
      then HttpError HTS.status422 $ "422 Unprocessable Entity: Missing request header \"" <> fromStrict (original name) <> "\""
      else HttpError HTS.status422 $ "422 Unprocessable Entity: Missing request header (one of " <> BSL.pack (show (name : others)) <> ")"
  f (UnprocessableHeaderValueError name) =
    HttpError HTS.status422 $ "422 Unprocessable Entity: request header \"" <> fromStrict (original name) <> "\" is invalid."
