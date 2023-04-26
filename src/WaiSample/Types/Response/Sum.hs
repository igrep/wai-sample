{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Ref. https://github.com/cdepillabout/world-peace/blob/0596da67d792ccf9f0ddbe44b5ce71b38cbde020/src/Data/WorldPeace/Union.hs
module WaiSample.Types.Response.Sum where

import           Data.Kind                    (Constraint, Type)
import           Data.Type.Bool               (If)
import           Data.Typeable                (Typeable)
import           GHC.TypeLits                 (ErrorMessage (..), TypeError)
import           Language.Haskell.TH          (Exp, Quote)
import           Language.Haskell.TH.Syntax   (Code (Code), Lift, lift,
                                               liftTyped, unsafeTExpCoerce)
import           WaiSample.Types.ContentTypes (HasContentTypes (contentTypes))
import           WaiSample.Types.Response     (DecodeByResponseSpec,
                                               EncodeByResponseSpec,
                                               FromRawResponse,
                                               RawResponse (RawResponse, rawStatusCode),
                                               Response (Response, responseObject),
                                               ResponseObject, ResponseSpec,
                                               ResponseType, ToRawResponse,
                                               decodeByResponseSpec,
                                               encodeByResponseSpec,
                                               fromRawResponse, toRawResponse)
import           WaiSample.Types.Status       (HasStatusCode (statusCodes))


-- [1, 2, 3] :: [Int]
-- [True, False, True] :: [Bool]
-- [Bool, Int, String] :: [Type]

-- data [a] = [] | a : [a]
--      ↓ 昇格
-- kind [a] = '[] | a ': [a]

-- data Bool = True | False
--      ↓ 昇格
-- kind Bool = 'True | 'False

{-
  Left (Right 1) :: Either (Either Bool Int) String
  That (This 1) :: Union '[String, Int]
  That (This 1) :: Union '[String, Int, Maybe Bool]
  That (This 1) :: Union '[]
                          String ': [Int]
                          String ': Int ': '[]
                          (String, Int)
                          ["abc", "def"] :: [String]
                          "abc" : ["def"]
-}

data Sum (as :: [Type]) where
  This :: a -> Sum (a ': as)
  That :: Sum as -> Sum (a ': as)

class LiftSum (as :: [Type]) where
  liftSum :: Quote m => Sum as -> m Exp

instance LiftSum '[] where
  liftSum = absurdSum

instance (Lift a, LiftSum as) => LiftSum (a ': as) where
  liftSum (This this) = [| This $(lift this) |]
  liftSum (That that) = [| That $(liftSum that) |]

instance LiftSum as => Lift (Sum as) where
  liftTyped x = Code $ unsafeTExpCoerce (liftSum x)
  lift = liftSum

instance Show (Sum '[]) where
  show _ = ""

-- E.g. That (That (This 90))
instance (Show a, Show (Sum as)) => Show (Sum (a ': as)) where
  show (This this) = "This " ++ show this
  show (That that) = "That (" ++ show that ++ ")"

instance Eq (Sum '[]) where
  _ == _ = False

instance (Eq a, Eq (Sum as)) => Eq (Sum (a ': as))where
  This x == This y = x == y
  That x == That y = x == y
  _ == _           = False

sumLift :: IsMember a as => a -> Sum as
sumLift = sumLift'


sumMatch :: IsMember a as => Sum as -> Maybe a
sumMatch = sumMatch'


sumChoose :: (a -> c) -> (Sum as -> c) -> Sum (a ': as) -> c
sumChoose onThis _ (This a) = onThis a
sumChoose _ onThat (That u) = onThat u


absurdSum :: Sum '[] -> a
absurdSum u = case u of {}


-- | This is a helpful 'Constraint' synonym to assert that @a@ is a member of
-- @as@.  You can see how it is used in functions like 'openSumLift'.
type IsMember (a :: Type) (as :: [Type]) = (CheckElemIsMember a as, SumElemAt a as (RIndex a as))


-- | This type family checks whether @a@ is inside @as@ and produces
-- compile-time error if not. Actually, 'RIndex' is enough for checking
-- if @a@ is a member of @as@. So 'CheckElemIsMember' is only for
-- better error reporting.
--
-- type family CheckElemIsMember :: Type -> [Type] -> Constraint where
type family CheckElemIsMember (a :: Type) (as :: [Type]) :: Constraint where
  CheckElemIsMember a as =
    If (Elem a as) (() :: Constraint) (TypeError (NoElementError a as))


-- | Type-level version of the 'elem' function.
--
-- >>> Refl :: Elem String '[Double, String, Char] :~: 'True
-- Refl
-- >>> Refl :: Elem String '[Double, Char] :~: 'False
-- Refl
-- type family Elem :: Type -> [Type] -> Bool where
type family Elem (a :: Type) (as :: [Type]) :: Bool where
  Elem _ '[]       = 'False
  Elem x (x ': xs) = 'True
  Elem x (y ': xs) = Elem x xs


-- | Text of the error message.
type NoElementError (r :: Type) (rs :: [Type]) =
        'Text "You require open sum type to contain the following element:"
  ':$$: 'Text "    " ':<>: 'ShowType r
  ':$$: 'Text "However, given list can store elements only of the following types:"
  ':$$: 'Text "    " ':<>: 'ShowType rs


-- | @'SumElemAt' a as i@ provides a way to potentially get an @a@ out of a
-- @'Sum' as@ ('sumMatch'). It also provides a way to create a
-- @'Sum' as@ from an @a@ ('sumLift').
--
-- This is safe because of the 'RIndex' contraint. This 'RIndex' constraint
-- tells us that there /actually is/ an @a@ in @as@ at index @i@.
--
-- As an end-user, you should never need to implement an additional instance of
-- this typeclass.
class (i ~ RIndex a as) => SumElemAt (a :: Type) (as :: [Type]) (i :: Nat) where
  sumLift' :: a -> Sum as
  sumMatch' :: Sum as -> Maybe a

instance SumElemAt a (a ': as) 'Z where
  -- sumLift' :: a -> Sum (a ': as)
  sumLift' = This
  -- sumMatch' :: Sum (a ': as) -> Maybe a
  sumMatch' (This x) = Just x
  sumMatch' (That _) = Nothing

instance
  ( RIndex a (b ': as) ~ 'S i
  , SumElemAt a as i
  ) => SumElemAt a (b ': as) ('S i) where
  -- sumLift' :: a -> Sum (b ': as)
  sumLift' = That . sumLift'
  -- sumMatch' :: Sum (b ': as) -> Maybe a
  sumMatch' (This _)  = Nothing
  sumMatch' (That xs) = sumMatch' xs


type family RIndex (a :: Type) (as :: [Type]) :: Nat where
  RIndex r (r ': rs) = 'Z
  RIndex r (s ': rs) = 'S (RIndex r rs)


-- | A mere approximation of the natural numbers. And their image as lifted by
-- @-XDataKinds@ corresponds to the actual natural numbers.
data Nat = Z | S Nat


class ResponseSpecAll (resSpecs :: [Type]) where
  type AllResponseTypes resSpecs :: [Type]
  type AllResponseObjects resSpecs :: [Type]

instance (ResponseSpecAll '[]) where
  type AllResponseTypes '[] = '[]
  type AllResponseObjects '[] = '[]

instance
  ( ResponseSpec resSpec
  , ResponseSpecAll resSpecs
  ) => ResponseSpecAll (resSpec ': resSpecs) where
  type AllResponseTypes (resSpec ': resSpecs) = ResponseType resSpec ': AllResponseTypes resSpecs
  type AllResponseObjects (resSpec ': resSpecs) = ResponseObject resSpec ': AllResponseObjects resSpecs

instance ResponseSpecAll resSpecs => ResponseSpec (Sum resSpecs) where
  type ResponseType (Sum resSpecs) = Sum (AllResponseTypes resSpecs)
  type ResponseObject (Sum resSpecs) = Sum (AllResponseObjects resSpecs)


instance DecodeByResponseSpec (Sum '[]) where
  decodeByResponseSpec _ _ = fail "DecodeByResponseSpec: Impossible: Empty Sum"

instance
  ( ResponseSpec resSpec
  , Typeable (ResponseObject resSpec)
  , DecodeByResponseSpec resSpec
  , DecodeByResponseSpec (Sum resSpecs)
  , ResponseSpecAll resSpecs
  ) => DecodeByResponseSpec (Sum (resSpec ': resSpecs)) where
  decodeByResponseSpec mt (This resObj)  = decodeByResponseSpec @resSpec mt resObj
  decodeByResponseSpec mt (That resObjs) = decodeByResponseSpec @(Sum resSpecs) mt resObjs


instance ToRawResponse (Sum '[]) where
  toRawResponse _ _ = fail "ToRawResponse: Impossible: Empty Sum"

instance
  ( Typeable resObj
  , HasContentTypes resTyp
  , ToRawResponse (resTyp, resObj)
  , ToRawResponse (Sum resSpecs)
  ) => ToRawResponse (Sum ((resTyp, resObj) ': resSpecs)) where
  toRawResponse mt (This resObj)  = toRawResponse @(resTyp, resObj) mt resObj
  toRawResponse mt (That resObjs) = toRawResponse @(Sum resSpecs) mt resObjs

instance
  ( Typeable resObj
  , HasContentTypes resTyp
  , ToRawResponse (resTyp, resObj)
  , ToRawResponse (Sum resSpecs)
  ) => ToRawResponse (Sum (Response resTyp resObj ': resSpecs)) where
  toRawResponse mt (This resObj)  = toRawResponse @(resTyp, resObj) mt $ responseObject resObj
  toRawResponse mt (That resObjs) = toRawResponse @(Sum resSpecs) mt resObjs


instance EncodeByResponseSpec (Sum '[]) where
  encodeByResponseSpec _ _ = fail "EncodeByResponseSpec: Impossible: Empty Sum"

instance
  ( ResponseSpec resSpec
  , Typeable (ResponseObject resSpec)
  , HasStatusCode (ResponseType resSpec)
  , HasContentTypes (ResponseType resSpec)
  , EncodeByResponseSpec resSpec
  , EncodeByResponseSpec (Sum resSpecs)
  , ResponseSpecAll resSpecs
  ) => EncodeByResponseSpec (Sum (resSpec ': resSpecs)) where
  encodeByResponseSpec mt rr@RawResponse { rawStatusCode } =
    if rawStatusCode `elem` statusCodes @(ResponseType resSpec)
        && mt `elem` contentTypes @(ResponseType resSpec)
      then This <$> encodeByResponseSpec @resSpec mt rr
      else That <$> encodeByResponseSpec @(Sum resSpecs) mt rr

instance FromRawResponse (Sum '[]) where
  fromRawResponse _ _ = fail "FromRawResponse: Impossible: Empty Sum"

instance
  ( Typeable resObj
  , HasContentTypes resTyp
  , FromRawResponse (resTyp, resObj)
  , FromRawResponse (Sum resSpecs)
  ) => FromRawResponse (Sum ((resTyp, resObj) ': resSpecs)) where
  fromRawResponse mt rr@RawResponse { rawStatusCode } =
    if rawStatusCode `elem` statusCodes @resTyp
        && mt `elem` contentTypes @resTyp
      then This <$> fromRawResponse @(resTyp, resObj) mt rr
      else That <$> fromRawResponse @(Sum resSpecs) mt rr

instance
  ( Typeable resObj
  , HasContentTypes resTyp
  , FromRawResponse (resTyp, resObj)
  , FromRawResponse (Sum resSpecs)
  ) => FromRawResponse (Sum (Response resTyp resObj ': resSpecs)) where
  fromRawResponse mt rr = do
    resObj <- fromRawResponse @(Sum ((resTyp, resObj) ': resSpecs)) mt rr
    case resObj of
      This thisObj -> return . This $ Response @resTyp thisObj
      That _that   -> That <$> fromRawResponse @(Sum resSpecs) mt rr


instance HasStatusCode (Sum '[]) where
  statusCodes = []

instance
  ( HasStatusCode resTyp
  , HasStatusCode (Sum resTyps)
  ) => HasStatusCode (Sum (resTyp ': resTyps)) where
  statusCodes = statusCodes @resTyp ++ statusCodes @(Sum resTyps)


instance HasContentTypes (Sum '[]) where
  contentTypes = []

instance
  ( LiftSum resTyps
  , HasContentTypes resTyp
  , HasContentTypes (Sum resTyps)
  ) => HasContentTypes (Sum (resTyp ': resTyps)) where
  contentTypes = contentTypes @resTyp ++ contentTypes @(Sum resTyps)
