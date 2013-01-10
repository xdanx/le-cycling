module Checks where

import Control.Monad.Random
import Data.Maybe
import Data.Sequence as Sequence
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
  shrink = shrinkNothing

instance Arbitrary Pack where
  arbitrary = do
    a@(l:p) <- listOf1 arbitrary
    tLead <- elements [0..4]
    tBreak <- elements [1..3]
    let pid = unsafePerformIO $ newID
    elements [Pack tLead l (Sequence.fromList p) pid, Breakaway (Sequence.fromList a) tBreak pid]
  shrink = shrinkNothing


instance Arbitrary Race where
         arbitrary = do
                   packs <- listOf1 arbitrary
                   sprinters <- listOf arbitrary
                   return $ Race 0 10000 packs sprinters []
         shrink = shrinkNothing

instance (Arbitrary a) => Arbitrary (Seq a) where
         arbitrary = arbitrary >>= return . Sequence.fromList
         shrink = shrinkNothing

lenPack :: Pack -> Int
lenPack = Sequence.length . getPack

lenRace :: Race -> Int
lenRace (Race _ _ runners sprinters win ) = (sum . map lenPack $ runners) + Prelude.length sprinters + Prelude.length win

testListCyclist :: ([Cyclist] -> [Cyclist]) -> [Cyclist] -> Bool
testListCyclist f l = (Prelude.length (f l) == Prelude.length l)

testPack :: (Pack -> Pack) -> Pack -> Bool
testPack f l = (lenPack $ l) == (lenPack . f $ l)

testListPack :: ([Pack] -> [Pack]) -> [Pack] -> Bool
testListPack f l = (sum . map lenPack $ l) == (sum . map lenPack . f $ l)

testListPackM :: (Pack -> RandT StdGen IO [Pack]) -> Pack -> Bool
testListPackM f l = (lenPack l) == (sum . map lenPack . unsafePerformIO . flip evalRandT g . f $ l)
              where g = unsafePerformIO $ getStdGen

testRace :: (Race -> Race) -> Race -> Bool
testRace f l = (lenRace l) == (lenRace . f $ l)

testRaceM :: (Race -> RandT StdGen IO Race) -> Race -> Bool
testRaceM f l = (lenRace l) == (lenRace . unsafePerformIO . flip evalRandT g . f $ l)
          where g = unsafePerformIO $ getStdGen


testUpdatePack :: Pack -> Bool
testUpdatePack p = (lenPack p) == ((Prelude.length (sprint ++ win)) + lp')
               where (p', sprint, win) = updatePack 10000 p
                     lp' = if isJust p'
                           then lenPack . fromJust $ p'
                           else 0

testSeqListCyclist :: (Seq Cyclist -> [Seq Cyclist]) -> Seq Cyclist -> Bool
testSeqListCyclist f s = (Sequence.length s) == (sum . map Sequence.length . f $ s)
-- all testEq1 []



