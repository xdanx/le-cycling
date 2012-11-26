module ID (
   newID,
   
 ) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.Trans
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE counter #-}
counter :: TVar Int
counter = unsafePerformIO (newTVarIO 0)

newID ::(MonadIO m) => m Int
newID = liftIO . atomically $ do
  val <- readTVar counter
  let next = val+1
  writeTVar counter $! next
  return val

resetID :: (MonadIO m) => m ()
resetID = liftIO . atomically . writeTVar counter $! 0
