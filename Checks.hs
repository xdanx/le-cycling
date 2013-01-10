module Checks where

import Test.QuickCheck

import Simulation
import Pack
import Cyclist


instance Arbitrary Cyclist where
  arbitrary = 
    _uid <- undefined
    _pmax <- choose (0, 2.6)
    _ 
    Cyclist {uid = ,
             pmax = choose ()}
  
  
  shrink = :[]

