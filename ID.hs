{- ID.hs
Module to provide unique IDs for the packs.
-}

module ID (
   newID,
   resetID
 ) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.Trans
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE counter #-}
counter :: TVar Int
counter = unsafePerformIO (newTVarIO 0)

-- Return a new unused ID
newID ::(MonadIO m) => m Int
newID = liftIO . atomically $ do
  val <- readTVar counter
  let next = val+1
  writeTVar counter $! next
  return val

-- Resets the counter, the next call to newID will give 0. 
resetID :: (MonadIO m) => m ()
resetID = liftIO . atomically . writeTVar counter $! 0
