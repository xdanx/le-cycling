module Race where

import Cyclist

data Race = Race !Int !Int ![Cyclist] ![Cyclist]
     deriving (Show)

