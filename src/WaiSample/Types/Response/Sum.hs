{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

-- | Ref. https://github.com/cdepillabout/world-peace/blob/0596da67d792ccf9f0ddbe44b5ce71b38cbde020/src/Data/WorldPeace/Union.hs
module WaiSample.Types.Response.Sum where

import           Data.Kind (Constraint)
import           Data.Type.Bool (If)
import           GHC.TypeLits (ErrorMessage(..), TypeError)


-- [1, 2, 3] :: [Int]
-- [True, False, True] :: [Bool]
-- [Bool, Int, String] :: [*]

-- data [a] = [] | a : [a]
-- kind [a] = '[] | a ': [a]

data Sum (as :: [*]) where
  This :: a -> Sum (a ': as)
  That :: Sum as -> Sum (a ': as)


-- | This is a helpful 'Constraint' synonym to assert that @a@ is a member of
-- @as@.  You can see how it is used in functions like 'openUnionLift'.
type IsMember (a :: *) (as :: [*]) = (CheckElemIsMember a as, UElem a as (RIndex a as))


-- | This type family checks whether @a@ is inside @as@ and produces
-- compile-time error if not.
type family CheckElemIsMember :: * -> [*] -> Constraint where
  CheckElemIsMember a as =
    If (Elem a as) (() :: Constraint) (TypeError (NoElementError a as))


-- | Type-level version of the 'elem' function.
--
-- >>> Refl :: Elem String '[Double, String, Char] :~: 'True
-- Refl
-- >>> Refl :: Elem String '[Double, Char] :~: 'False
-- Refl
type family Elem :: * -> [*] -> Bool where
  Elem _ '[]       = 'False
  Elem x (x ': xs) = 'True
  Elem x (y ': xs) = Elem x xs


-- | Text of the error message.
type NoElementError (r :: *) (rs :: [*]) =
        'Text "You require open sum type to contain the following element:"
  ':$$: 'Text "    " ':<>: 'ShowType r
  ':$$: 'Text "However, given list can store elements only of the following types:"
  ':$$: 'Text "    " ':<>: 'ShowType rs
