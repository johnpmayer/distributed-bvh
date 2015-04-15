
{-# OPTIONS -Wall #-}

module DistributedBVH where

import Control.Concurrent
import Linear.V2

-- Bounds2

data Bounds2 a = Bounds2 !(V2 a) !(V2 a) deriving
  (Eq,Ord,Show,Read)

area :: Num a => Bounds2 a -> a
area (Bounds2 (V2 x1 y1) (V2 x2 y2)) =
  abs $ (x2 - x1) * (y2 - y1)

between :: Ord a => V2 a -> V2 a -> V2 a -> Bool
between (V2 x1 y1) (V2 x2 y2) (V2 x3 y3) =
  x1 <= x3 && x3 <= x2 && y1 <= y3 && y3 <= y2

intersect :: Ord a => Bounds2 a -> Bounds2 a -> Bool
intersect (Bounds2 lo1 hi1) (Bounds2 lo2 hi2) =
  between lo1 hi1 lo2 || between lo1 hi1 hi2

minStrict :: Ord a => V2 a -> V2 a -> V2 a
minStrict (V2 x1 y1) (V2 x2 y2) = V2 (min x1 x2) (min y1 y2)

maxStrict :: Ord a => V2 a -> V2 a -> V2 a
maxStrict (V2 x1 y1) (V2 x2 y2) = V2 (max x1 x2) (max y1 y2)

union :: Ord a => Bounds2 a -> Bounds2 a -> Bounds2 a
union (Bounds2 lo1 hi1) (Bounds2 lo2 hi2) = 
  Bounds2 (minStrict lo1 lo2) (maxStrict hi1 hi2)

data Command
data Query

data Node = Node
  { commands :: MVar Command
  }

data NodeState = NodeState
  { children :: [Node]
  }

foreverWith :: (Monad m) => (a -> m a) -> a -> m ()
foreverWith step state =
  step state >>= foreverWith step

nodeStep :: Node -> NodeState -> IO NodeState
nodeStep node state = do
  _c <- takeMVar (commands node)
  return state

startNode :: IO Node
startNode = do
  cs <- newEmptyMVar
  let node = Node cs
  let initial = NodeState []
  _nodeThread <- forkIO $ foreverWith (nodeStep node) initial
  return $ Node cs
