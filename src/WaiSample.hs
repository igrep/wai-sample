{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

module WaiSample
  ( Handler (..)
  , handler

  , get
  , post
  , put
  , delete
  , patch

  , getWith
  , postWith
  , putWith
  , deleteWith
  , patchWith

  , root
  , path
  , paramPiece
  , decimalPiece

  , module WaiSample.Types

  , showHandlerSpec
  , showRoutes
  ) where

import           Data.Proxy                (Proxy (Proxy))
import qualified Data.Text                 as T
import           Data.Typeable             (Typeable, typeRep)
import           Network.HTTP.Types.Method (Method, methodDelete, methodGet,
                                            methodPatch, methodPost, methodPut)
import           WaiSample.Routes
import           WaiSample.Types


showHandlerSpec :: Handler -> T.Text
showHandlerSpec (Handler resSpec name method tbl (_opts :: EndpointOptions q h) _hdl) =
  T.pack name <> " " <> T.pack (show method) <> " " <> showRoute tbl <> "\n"
    <> "  Request:\n"
    <> "    Query Params: " <> showQueryParamsType @q <> "\n"
    <> "    Headers: " <> showRequestHeadersType @h <> "\n"

    -- TODO: More HTTP response specific information
    <> "  Response: " <> T.pack (show $ typeRep resSpec) <> "\n"
  -- showRoutes が出す情報に加えて、次の情報を適当なフォーマットで返す
  --   レスポンスの情報: ステータスコード毎のレスポンスボディの型（Content-Type）、レスポンスヘッダーの型
  --   リクエストの情報: リクエストヘッダーの型


showRoutes :: [Handler] -> T.Text
showRoutes = T.intercalate "\n" . map f
 where
  f :: Handler -> T.Text
  f (Handler _resSpec _name _method tbl _opts _hdl) = showRoute tbl


handler
  :: forall resSpec p q h.
  ( ToRawResponse resSpec
  , FromRawResponse resSpec
  , Typeable resSpec
  , Typeable (ResponseObject resSpec)
  , HasStatusCode (ResponseType resSpec)
  , HasContentTypes (ResponseType resSpec)
  , Typeable q
  , ToQueryParams q
  , FromQueryParams q
  , ShowQueryParamsType q
  , Typeable h
  , ToRequestHeaders h
  , FromRequestHeaders h
  , ShowRequestHeadersType h
  )
  => String -> Method -> Route p -> EndpointOptions q h -> Responder p q h (ResponseObject resSpec) -> Handler
handler = Handler (Proxy :: Proxy resSpec)


get, post, put, delete, patch
  :: forall resSpec p.
  ( ToRawResponse resSpec
  , FromRawResponse resSpec
  , Typeable resSpec
  , Typeable (ResponseObject resSpec)
  , HasStatusCode (ResponseType resSpec)
  , HasContentTypes (ResponseType resSpec)
  )
  => String -> Route p -> SimpleResponder p (ResponseObject resSpec) -> Handler
get name route respond   = getWith @resSpec @p name route options (\p _reqInfo -> respond p)
post name route respond   = postWith @resSpec @p name route options (\p _reqInfo -> respond p)
put name route respond    = putWith @resSpec @p name route options (\p _reqInfo -> respond p)
delete name route respond = deleteWith @resSpec @p name route options (\p _reqInfo -> respond p)
patch name  route respond = patchWith @resSpec @p name route options (\p _reqInfo -> respond p)


getWith, postWith, putWith, deleteWith, patchWith
  :: forall resSpec p q h.
  ( ToRawResponse resSpec
  , FromRawResponse resSpec
  , Typeable resSpec
  , Typeable (ResponseObject resSpec)
  , HasStatusCode (ResponseType resSpec)
  , HasContentTypes (ResponseType resSpec)
  , Typeable q
  , ToQueryParams q
  , FromQueryParams q
  , ShowQueryParamsType q
  , Typeable h
  , ToRequestHeaders h
  , FromRequestHeaders h
  , ShowRequestHeadersType h
  )
  => String -> Route p -> EndpointOptions q h -> Responder p q h (ResponseObject resSpec) -> Handler
getWith name    = handler @resSpec @p name methodGet
postWith name   = handler @resSpec @p name methodPost
putWith name    = handler @resSpec @p name methodPut
deleteWith name = handler @resSpec @p name methodDelete
patchWith name  = handler @resSpec @p name methodPatch
