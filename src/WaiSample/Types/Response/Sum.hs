{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

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
-- @as@.  You can see how it is used in functions like 'openUnionLift'.
type IsMember (a :: *) (as :: [*]) = (CheckElemIsMember a as, UElem a as (RIndex a as))
-- ConstraintKinds


-- | This type family checks whether @a@ is inside @as@ and produces
-- compile-time error if not.
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
