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
       then lift . lift . putStrLn $ "Warning Can't parse " ++ show (map snd failed)
       else return ()
    let teams = (map ((map read) . fromJust) (filter isJust str_teams''))::[[Int]]
    concatMapM (\[t,n] -> makeCyclists t n (defaultPop str_profile) str_attrs) teams

makeCyclists :: Int -> Int -> Population -> String -> RandT StdGen (StateT Int IO) [Cyclist]
makeCyclists t n pop ln = do
  let attr_parse = map (\(x,_:y) -> (x,y)) . map (break (==':')) . words $ ln
      infs = ["max10", "e_rem", "c_b", "c_t", "breakaway", "speed", "distance", "t_lead"]
      [mmax10, me_rem, mc_b, mc_t, mbreakaway, mspeed, mdistance, mt_lead] =  (map (flip lookup attr_parse) infs)
  replicateM n (do
                   max10 <- getMax10 pop mmax10
                   let e_rem = getE_rem me_rem
                   c_b <- getC pop mc_b
                   c_t <- getC pop mc_t
                   let breakaway = getI mbreakaway
                       speed = getD mspeed
                       distance = getD mdistance
                       t_lead = getI mt_lead
                   uid <- lift get
                   lift . put $ uid + 1
                   return (Cyclist {Cyclist.id = uid, max10 = max10, s_m = exp 2.478, e_rem = e_rem, c_b = c_b, c_t = c_t, breakaway = breakaway, speed = speed, distance = distance, position = 1, t_lead = t_lead, team = t, t_coop = True, b_coop = True}) 
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
