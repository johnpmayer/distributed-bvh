
module DistributedBVH where

import Control.Concurrent
import Control.Concurrent.MVar

data Command
data Query

data Node = Node
  { insert :: MVar Command
  }

node :: IO Node
node = do
  insert' <- newEmptyMVar
  return $ Node insert'
