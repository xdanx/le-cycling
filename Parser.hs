module Parser where

import Control.Monad.Random
import Control.Monad.Trans
import Data.List
import Data.List.Split

import Cyclist
import Population
import Simulation

genRace :: String -> IO Race
genRace n = do
  f <- readFile n 
  g <- getStdGen
  (r, _) <- runRandT (parse f) g
  return r

parse :: String -> RandT StdGen (StateT IO Race
parse [] = error "Empty parse file"
parse f = do
  let h:c = lines f
      len = read h :: Int
  cs <- sequence . map parseLine $ c
  let (s, r) = partition (\c -> fromIntegral len - distance c < 5000) . concat $ cs 
  return (Race len 0 r s [])
                
parseLine :: String -> RandT StdGen IO [Cyclist]
parseLine l = do
    let [teams, profile, attrs] = splitOn "|" l
    return []  


defaultPop :: String -> Population
defaultPop "avg" = avg
defaultPop _ = undefined
