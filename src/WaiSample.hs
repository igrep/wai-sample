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

  , showRoutes
  ) where

import           Data.Proxy                (Proxy (Proxy))
import qualified Data.Text                 as T
import           Data.Typeable             (Typeable)
import           Network.HTTP.Types.Method (Method, methodDelete, methodGet,
                                            methodPatch, methodPost, methodPut)
import           WaiSample.Routes
import           WaiSample.Types


showHandlerSpec :: Handler -> T.Text
showHandlerSpec _ =
  -- showRoutes が出す情報に加えて、次の情報を適当なフォーマットで返す
  --   レスポンスの情報: ステータスコード毎のレスポンスボディの型（Content-Type）、レスポンスヘッダーの型
  --   リクエストの情報: リクエストヘッダーの型
  error "showHandlerSpec is not defined yet!"


showRoutes :: [Handler] -> T.Text
showRoutes = ("/" <>) . T.intercalate "\n/" . map f
 where
  f :: Handler -> T.Text
  f (Handler _resSpec _name _method tbl _opts _hdl) = showRoutes' tbl


handler
  :: forall resSpec a h.
  ( ToRawResponse resSpec
  , FromRawResponse resSpec
  , Typeable resSpec
  , Typeable (ResponseObject resSpec)
  , HasStatusCode (ResponseType resSpec)
  , HasContentTypes (ResponseType resSpec)
  , Typeable h
  , ToRequestHeaders h
  , FromRequestHeaders h
  )
  => String -> Method -> Route a -> EndpointOptions h -> Responder a h (ResponseObject resSpec) -> Handler
handler = Handler (Proxy :: Proxy resSpec)


get, post, put, delete, patch
  :: forall resSpec a.
  ( ToRawResponse resSpec
  , FromRawResponse resSpec
  , Typeable resSpec
  , Typeable (ResponseObject resSpec)
  , HasStatusCode (ResponseType resSpec)
  , HasContentTypes (ResponseType resSpec)
  )
  => String -> Route a -> SimpleResponder a (ResponseObject resSpec) -> Handler
get name route respond   = getWith @resSpec @a name route options (\p _reqInfo -> respond p)
post name route respond   = postWith @resSpec @a name route options (\p _reqInfo -> respond p)
put name route respond    = putWith @resSpec @a name route options (\p _reqInfo -> respond p)
delete name route respond = deleteWith @resSpec @a name route options (\p _reqInfo -> respond p)
patch name  route respond = patchWith @resSpec @a name route options (\p _reqInfo -> respond p)


getWith, postWith, putWith, deleteWith, patchWith
  :: forall resSpec a h.
  ( ToRawResponse resSpec
  , FromRawResponse resSpec
  , Typeable resSpec
  , Typeable (ResponseObject resSpec)
  , HasStatusCode (ResponseType resSpec)
  , HasContentTypes (ResponseType resSpec)
  , Typeable h
  , ToRequestHeaders h
  , FromRequestHeaders h
  )
  => String -> Route a -> EndpointOptions h -> Responder a h (ResponseObject resSpec) -> Handler
getWith name    = handler @resSpec @a name methodGet
postWith name   = handler @resSpec @a name methodPost
putWith name    = handler @resSpec @a name methodPut
deleteWith name = handler @resSpec @a name methodDelete
patchWith name  = handler @resSpec @a name methodPatch
