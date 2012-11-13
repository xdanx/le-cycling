module Parser where

import Cyclist
import Simulation

genRace :: String -> IO Race
genRace n = readFile n >>= return . parse

parse :: String -> Race
parse x = Race 0 0 [] [] []




