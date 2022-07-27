{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Ref. https://github.com/cdepillabout/world-peace/blob/0596da67d792ccf9f0ddbe44b5ce71b38cbde020/src/Data/WorldPeace/Union.hs
module WaiSample.Types.Response.Sum where

import           Data.Kind                    (Constraint)
import           Data.Proxy                   (Proxy (Proxy))
import           Data.Type.Bool               (If)
import           Data.Typeable                (Typeable)
import           GHC.TypeLits                 (ErrorMessage (..), TypeError)
import           Language.Haskell.TH          (ExpQ)
import           Language.Haskell.TH.Syntax   (Lift, lift, liftTyped,
                                               unsafeTExpCoerce)
import           Network.HTTP.Media           (MediaType)
import qualified Network.HTTP.Types.Status    as HTS
import           WaiSample.Types.ContentTypes (HasContentTypes (contentTypes))
import           WaiSample.Types.Response     (RawResponse, ResponseObject,
                                               ResponseSpec, ToRawResponse,
                                               toRawResponse)
import           WaiSample.Types.Status       (HasStatusCode (statusCodes))


-- [1, 2, 3] :: [Int]
-- [True, False, True] :: [Bool]
-- [Bool, Int, String] :: [*]

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

data Sum (as :: [*]) where
  This :: a -> Sum (a ': as)
  That :: Sum as -> Sum (a ': as)

class LiftSum (as :: [*]) where
  liftSum :: Sum as -> ExpQ

instance LiftSum '[] where
  liftSum = absurdSum

instance (Lift a, LiftSum as) => LiftSum (a ': as) where
  liftSum (This this) = [| This $(lift this) |]
  liftSum (That that) = [| That $(liftSum that) |]

instance LiftSum as => Lift (Sum as) where
  liftTyped x = unsafeTExpCoerce (liftSum x)
  lift = liftSum


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
type IsMember (a :: *) (as :: [*]) = (CheckElemIsMember a as, SumElemAt a as (RIndex a as))


-- | This type family checks whether @a@ is inside @as@ and produces
-- compile-time error if not. Actually, 'RIndex' is enough for checking
-- if @a@ is a member of @as@. So 'CheckElemIsMember' is only for
-- better error reporting.
--
-- type family CheckElemIsMember :: * -> [*] -> Constraint where
type family CheckElemIsMember (a :: *) (as :: [*]) :: Constraint where
  CheckElemIsMember a as =
    If (Elem a as) (() :: Constraint) (TypeError (NoElementError a as))


-- | Type-level version of the 'elem' function.
--
-- >>> Refl :: Elem String '[Double, String, Char] :~: 'True
-- Refl
-- >>> Refl :: Elem String '[Double, Char] :~: 'False
-- Refl
-- type family Elem :: * -> [*] -> Bool where
type family Elem (a :: *) (as :: [*]) :: Bool where
  Elem _ '[]       = 'False
  Elem x (x ': xs) = 'True
  Elem x (y ': xs) = Elem x xs


-- | Text of the error message.
type NoElementError (r :: *) (rs :: [*]) =
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
class (i ~ RIndex a as) => SumElemAt (a :: *) (as :: [*]) (i :: Nat) where
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


type family RIndex (a :: *) (as :: [*]) :: Nat where
  RIndex r (r ': rs) = 'Z
  RIndex r (s ': rs) = 'S (RIndex r rs)


-- | A mere approximation of the natural numbers. And their image as lifted by
-- @-XDataKinds@ corresponds to the actual natural numbers.
data Nat = Z | S Nat


class HasStatusCodesAll (as :: [*]) where
  allStatusCodes :: Proxy as -> [HTS.Status]

instance HasStatusCodesAll '[] where
  allStatusCodes _ = []

instance (HasStatusCode a, HasStatusCodesAll as) => HasStatusCodesAll (a ': as) where
  allStatusCodes _ =
    statusCodes (Proxy :: Proxy a) ++ allStatusCodes (Proxy :: Proxy as)

instance HasStatusCodesAll as => HasStatusCode (Sum as) where
  statusCodes _ = allStatusCodes (Proxy :: Proxy as)


class HasContentTypesAll (as :: [*]) where
  allContentTypes :: Proxy as -> [MediaType]

instance HasContentTypes a => HasContentTypesAll '[a] where
  allContentTypes _ = contentTypes (Proxy :: Proxy a)

instance (HasContentTypes a, HasContentTypesAll as) => HasContentTypesAll (a ': as) where
  allContentTypes _ = contentTypes (Proxy :: Proxy a) <> allContentTypes (Proxy :: Proxy as)

instance (LiftSum as, HasContentTypesAll as) => HasContentTypes (Sum as) where
  contentTypes _ = allContentTypes (Proxy :: Proxy as)


class ResponseSpecAll (resSpecs :: [*]) where
  type AllResponseObjects resSpecs :: [*]

instance (ResponseSpecAll '[]) where
  type AllResponseObjects '[] = '[]

instance
  ( ResponseSpec (resTyp, resObj)
  , ResponseSpecAll resSpecs
  ) => ResponseSpecAll ((resTyp, resObj) ': resSpecs) where
  type AllResponseObjects ((resTyp, resObj) ': resSpecs) = resObj ': AllResponseObjects resSpecs

instance ResponseSpecAll resSpecs => ResponseSpec (Sum resSpecs) where
  type ResponseObject (Sum resSpecs) = Sum (AllResponseObjects resSpecs)

instance ToRawResponse (Sum '[]) where
  toRawResponse _ _ = fail "Impossible: Empty Sum"

instance
  ( Typeable resObj
  , ToRawResponse (resTyp, resObj)
  , ToRawResponse (Sum resSpecs)
  , ResponseSpecAll resSpecs
  ) => ToRawResponse (Sum ((resTyp, resObj) ': resSpecs)) where
  toRawResponse mt (This resObj)  = _
  toRawResponse mt (That resObjs) = _
