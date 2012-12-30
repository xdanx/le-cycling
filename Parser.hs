module Parser where

import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans
import Control.Monad.State
import Data.Dynamic
import Data.List
import Data.List.Split
import Data.Maybe
import Data.String.Utils
import Text.Regex

import Cyclist
import ID
import Pack
import Population
import Simulation
import Stats
import Utils
import Units

genRace :: String -> IO Race
genRace n = do
  f <- readFile n 
  getStdGen >>= evalRandT (parse f)

parse :: String -> RandT StdGen IO Race
parse [] = error "Empty parse file"
parse f = do
  let h:c = lines f
      len = read h :: Meters
  cs <- sequence . map parseLine $ c
  let (s, r) = partition (\c -> (len - distance c) < 5000) . concat $ cs 
  return (Race 0 len (getPacks r) s [])
                
parseLine :: String -> RandT StdGen IO [Cyclist]
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

--Parses a cyclist representation and produces n cyclists of this kind. TESTED
makeCyclists :: Int -> Int -> Population -> String -> RandT StdGen IO [Cyclist]
makeCyclists t n pop ln = do
  let attr_parse = map (\(x,_:y) -> (x,y)) . map (break (==':')) . words $ ln
      infs = ["pmax", "pcp", "usedEnergy", "energyLim", "genCProb", "teamCProb", "speed", "distance"]
      mParse = map (flip lookup attr_parse) infs
      transMakers = [setPmax, setPcp, setUsedEnergy, setEnergyLim, setSpeed, setDistance]
      trans = zipWith (\f m -> fromMaybe id (m >>= return . flip f . read)) transMakers mParse 
  replicateM n (genCyclist t pop >>= return . (foldl (.) id trans))
  

defaultPop :: String -> Population
defaultPop s 
  | (strip s) == "avg" = avg
  | otherwise = undefined
