{-# LANGUAGE DataKinds #-}

module WaiSample.TypesSpec
  ( spec
  ) where

import           Test.Syd        (Spec, describe, it, shouldBe, shouldNotBe)

import           WaiSample.Types (Sum (That, This))

type Subject = Sum '[Int, Bool, Char]

spec :: Spec
spec = describe "Sum (a ': as)" $
  describe "Eq (Sum (a ': as))" $ do
    it "If both operands are wrapped by the same number of the `That`'s, compare the values of the `This`'s." $ do
      (This 1 :: Subject) `shouldBe` This 1
      (That (This True) :: Subject) `shouldBe` That (This True)
      (That (That (This 'b')) :: Subject) `shouldBe` That (That (This 'b'))

      (This 1 :: Subject) `shouldNotBe` This 2
      (That (This True) :: Subject) `shouldNotBe` That (This False)
      (That (That (This 'b')) :: Subject) `shouldNotBe` That (That (This 'c'))

    it "If the number of the `That`'s wrapping the `This`'s differs from each other operand, returns False" $ do
      -- 1 v.s. 0
      (That (This True) :: Subject) `shouldNotBe` This 2
      This 2 `shouldNotBe` (That (This True) :: Subject)

      -- 2 v.s. 0
      (That (That (This 'c')) :: Subject) `shouldNotBe` This 2
      This 2 `shouldNotBe` (That (That (This 'c')) :: Subject)

      -- 2 v.s. 1
      (That (That (This 'c')) :: Subject) `shouldNotBe` That (This False)
      That (This False) `shouldNotBe` (That (That (This 'c')) :: Subject)
