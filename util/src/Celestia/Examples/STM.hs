
module Celestia.Examples.STM where

import Control.Monad (replicateM_)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (STM, newTVar, readTVar, atomically, writeTVar)


-- STM Example
-- Taken from: https://wiki.haskell.org/Simple_STM_example
example1 :: IO ()
example1 = do
  shared <- atomically $ newTVar 0
  before <- atomically . readTVar $ shared
  forkIO $ replicateM_ 25 $ (atomically . readTVar $ shared) >>= print >> threadDelay (20 * 1000)
  forkIO $ replicateM_ 10 $ (atomically . readTVar $ shared) >>= return . writeTVar shared . (+) 2 >> threadDelay (50 * 1000)
  forkIO $ replicateM_ 20 $ (atomically . readTVar $ shared) >>= return . writeTVar shared . pred  >> threadDelay (25 * 1000)
  threadDelay (800 * 1000)
  after <- atomically . readTVar $ shared
  putStrLn $ "Before: " ++ show before
  putStrLn $ "After:  " ++ show after



