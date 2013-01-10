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
