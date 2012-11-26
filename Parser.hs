module Parser where

import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans
import Control.Monad.State
import Data.List
import Data.List.Split
import Data.Maybe
import Data.String.Utils
import Text.Regex

import Cyclist
import Population
import Simulation
import Stats
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
  return (Race 0 len r s [])
                
parseLine :: String -> RandT StdGen (StateT Int IO) [Cyclist]
parseLine l = do
    let [str_teams, str_profile, str_attrs] = splitOn "|" l
        str_teams'  = words str_teams
        str_teams'' = map (matchRegex (mkRegex "([0-9]+):([0-9]+)")) str_teams'
        failed = filter (isNothing . fst) (zip str_teams'' str_teams')
    if not . null $ failed 
       then liftIO . putStrLn $ "Warning Can't parse " ++ show (map snd failed)
       else return ()
    let teams = (map ((map read) . fromJust) (filter isJust str_teams''))::[[Int]]
    concatMapM (\[t,n] -> makeCyclists t n (defaultPop str_profile) str_attrs) teams

makeCyclists :: Int -> Int -> Population -> String -> RandT StdGen (StateT Int IO) [Cyclist]
makeCyclists t n pop ln = do
  let attr_parse = map (\(x,_:y) -> (x,y)) . map (break (==':')) . words $ ln
      infs = ["max10", "tExh", "genCProb", "teamCProb", "breakaway", "speed", "distance", "tLead"]
      [mmax10, mtExh, mgenCProb, mteamCProb, mbreakaway, mspeed, mdistance, mtLead] =  (map (flip lookup attr_parse) infs)
  replicateM n (do
                   max10 <- getMax10 pop mmax10
                   let tExh = getE_rem mtExh
                   genCProb <- getC pop mgenCProb
                   teamCProb <- getC pop mteamCProb
                   let breakaway = getI mbreakaway
                       speed = getD mspeed
                       distance = getD mdistance
                       tLead = getI mtLead
                   uid <- lift get
                   lift . put $ uid + 1
                   return (Cyclist {Cyclist.id = uid, max10 = max10, speedM10 = exp 2.478, tExh = tExh, genCProb = genCProb, teamCProb = teamCProb, breakaway = breakaway, speed = speed, distance = distance, tLead = tLead, team = t, teamCoop = True, genCoop = True}) 
                   )
        
getMax10 :: Population -> Maybe String -> RandT StdGen (StateT Int IO) Double
getMax10 _ (Just x) = return . read $ x
getMax10 pop Nothing = normal . max10s $ pop

getE_rem :: Maybe String -> Double
getE_rem (Just x) = read x
getE_rem Nothing = 1/0

getC :: Population -> Maybe String -> RandT StdGen (StateT Int IO) Double
getC _ (Just x) = return . read $ x
getC pop Nothing = normal . coops $ pop

getI :: Maybe String -> Int
getI (Just x) = read x
getI Nothing = 0

getD :: Maybe String -> Double
getD (Just x) = read x
getD Nothing = 0
  
defaultPop :: String -> Population
defaultPop s 
  | (strip s) == "avg" = avg
  | otherwise = undefined
