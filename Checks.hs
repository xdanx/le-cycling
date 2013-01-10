module Checks where

import Test.QuickCheck

import Simulation
import Pack
import Cyclist


instance Arbitrary Cyclist where
  arbitrary = 
  shrink = :[]


instance Arbitrary Pack where
  arbitrary = 
  shrink = (:[])
  
