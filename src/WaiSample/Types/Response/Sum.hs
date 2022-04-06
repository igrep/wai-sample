{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Ref. https://github.com/cdepillabout/world-peace/blob/0596da67d792ccf9f0ddbe44b5ce71b38cbde020/src/Data/WorldPeace/Union.hs
module WaiSample.Types.Response.Sum where

import           Data.Kind      (Constraint)
import           Data.Type.Bool (If)
import           GHC.TypeLits   (ErrorMessage (..), TypeError)


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
  That (This 1) :: Union '[String, Int, Maybe Bool,]
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
class i ~ RIndex a as => SumElemAt (a :: *) (as :: [*]) (i :: Nat) where
  unionLift :: a -> Sum as
  unionMatch :: Sum as -> Maybe a

instance SumElemAt a (a ': as) 'Z where
  unionLift = This
  unionMatch (This x)  = Just x
  unionMatch (That xs) = Nothing

instance
  ( RIndex a (b ': as) ~ 'S i
  , SumElemAt a as i
  ) => SumElemAt a (b ': as) ('S i) where
  unionLift = That . unionLift
  unionMatch (This _)  = Nothing
  unionMatch (That xs) = unionMatch xs


type family RIndex (a :: *) (as :: [*]) :: Nat where
  RIndex r (r ': rs) = 'Z
  RIndex r (s ': rs) = 'S (RIndex r rs)


-- | A mere approximation of the natural numbers. And their image as lifted by
-- @-XDataKinds@ corresponds to the actual natural numbers.
data Nat = Z | S Nat
