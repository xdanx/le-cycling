module Parser where

import Control.Monad.Random
import Control.Monad.Trans
import Control.Monad.State
import Data.List
import Data.List.Split

import Cyclist
import Simulation

genRace :: String -> IO Race
genRace n = do
  f <- readFile n 
  g <- getStdGen
  ((r, _), _) <- flip runStateT 0 $ runRandT (parse f) g
  return r

parse :: String -> RandT StdGen (StateT Int IO) Race
parse [] = error "Empty parse file"
parse f = do
  let h:c = lines f
      len = read h :: Int
  cs <- sequence . map parseLine $ c
  let (s, r) = partition (\c -> fromIntegral len - distance c < 5000) . concat $ cs 
  return (Race len 0 r s [])
                
parseLine :: String -> RandT StdGen (StateT Int IO) [Cyclist]
parseLine l = do
    let [str_teams, str_profile, str_attrs] = splitOn "|" l
    
    return []  



