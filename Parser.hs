module Parser where

import Control.Monad.Random
import Control.Monad.Trans

import Cyclist
import Simulation

genRace :: String -> IO Race
genRace n = do
  f <- readFile n 
  g <- getStdGen
  runRandT g parse

parse :: String -> RandT StGen IO Race
parse [] = error "Empty parse file"
parse f = do
  let h:c = lines f
      len = read h :: Int
  cs <- sequence . map parseLine $ c
  let (s, r) = partition (\c -> len - distance c < 5000) cs 
  return (Race len 0 r s [])
                


parseLine :: String -> RandT StdGen IO Race
parseLine l = do
