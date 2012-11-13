module Parser where

import Control.Monad.Random
import Control.Monad.Trans
import Control.Monad.State
import Data.List
import Data.List.Split
import Data.Maybe
import Text.Regex


import Cyclist
import Population
import Simulation
import Utils

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
        str_teams'  = words str_teams
        str_teams'' = map (matchRegex (mkRegex "([0-9]+):([0-9]+)")) str_teams'
    lift . lift . putStrLn $ "Warning Can't parse " ++ show (map snd (filter (isNothing . fst) (zip str_teams'' str_teams')))
    let teams = (map ((map read) . fromJust) (filter isJust str_teams''))::[[Int]]
    
    return concatMapM (\[t,n] -> makeCyclist t n (defaultPop str_profile) str_attrs) teams


defaultPop :: String -> Population
defaultPop "avg" = avg
defaultPop _ = undefined
