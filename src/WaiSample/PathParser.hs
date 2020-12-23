{-# LANGUAGE OverloadedStrings #-}

module WaiSample.PathParser where

import           Data.Bifunctor (first)
import qualified Data.Text      as T
import qualified Data.Text.Read as TR

newtype PathParser a = PathParser { run :: [T.Text] -> Maybe (a, [T.Text]) }

instance Functor PathParser where
  fmap f p = PathParser $ \inp ->
    first f <$> run p inp

instance Applicative PathParser where
  pure x = PathParser $ \inp -> Just (x, inp)
  pf <*> px = PathParser $ \inp -> do
    (f, out) <- run pf inp
    run (fmap f px) out


anyPiece :: PathParser T.Text
anyPiece = PathParser $ \inp ->
  case inp of
      []       -> Nothing
      (p : ps) -> Just (p, ps)


pieceSatisfying :: (T.Text -> Bool) -> PathParser T.Text
pieceSatisfying predicate = PathParser $ \inp -> do
  (p, ps) <- run anyPiece inp
  if predicate p then Just (p, ps) else Nothing


piece :: T.Text -> PathParser T.Text
piece p = pieceSatisfying (== p)


-- piece "foo" *> piece "example" *> piece "users"
-- path "foo/example/users" *> decimalPiece

-- :id of /for/example/users/:id
decimalPiece :: PathParser Int
decimalPiece = PathParser $ \inp -> do
  (p, ps) <- run anyPiece inp
  case TR.decimal p of
      Right (i, "") -> Just (i, ps)
      Right _       -> Nothing
      Left _        -> Nothing
