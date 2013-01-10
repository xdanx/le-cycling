module Checks where

import Control.Monad.Random
import System.IO.Unsafe
import Test.QuickCheck
import Test.QuickCheck.Gen

import Coop
import Cyclist
import Pack
import Population
import Simulation


instance Arbitrary Cyclist where
  arbitrary = MkGen $ \x y -> unsafePerformIO . flip evalRandT x $ (genCyclist 1 (standardCoop, avg))
  shrink = (:[])

instance Arbitrary Race where
         arbitrary = do
                   packs <- listOf1 arbitrary
                   sprinters <- listOf1 arbitrary
                   return $ Race 0 10000 packs sprinters []
         shrink = (:[])
