module Checks where

import Control.Monad.Random
import Data.Sequence
import System.IO.Unsafe
import Test.QuickCheck
import Test.QuickCheck.Gen

import Coop
import Cyclist
import ID
import Pack
import Population
import Simulation


instance Arbitrary Cyclist where
  arbitrary = MkGen $ \x y -> unsafePerformIO . flip evalRandT x $ (genCyclist 1 (standardCoop, avg))
  shrink = (:[])

instance Arbitrary Pack where
  arbitrary = do
    a@(l:p) <- listOf1 arbitrary
    tLead <- elements [0..4]
    tBreak <- elements [1..3]
    let pid = unsafePerfomIO $ newID
    return . oneOf $ [Pack tLead l (fromList p) pid, 
                      Breakaway (fromList a) tBreak pid]
  shrink = (:[])
