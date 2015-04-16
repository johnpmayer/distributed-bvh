
{-# OPTIONS -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DistributedBVH where

import Control.Concurrent
import Linear.V2

-- TODO for testing, make these tunable...

minNodeSize, maxNodeSize :: Int
minNodeSize = 4
maxNodeSize = 8

-- Bounds2

data Bounds2 a = Bounds2 !(V2 a) !(V2 a) deriving
  (Eq,Show)

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
minStrict (V2 x1 y1) (V2 x2 y2) = 
  V2 (min x1 x2) (min y1 y2)

maxStrict :: Ord a => V2 a -> V2 a -> V2 a
maxStrict (V2 x1 y1) (V2 x2 y2) = 
  V2 (max x1 x2) (max y1 y2)

union :: Ord a => Bounds2 a -> Bounds2 a -> Bounds2 a
union (Bounds2 lo1 hi1) (Bounds2 lo2 hi2) = 
  Bounds2 (minStrict lo1 lo2) (maxStrict hi1 hi2)

data Command n = Insert [EntityLike n]
data Query

data Node n = Node (MVar (Command n)) 

class Entity e n where
  bounds :: e -> Bounds2 n

data EntityLike n 
 = forall e. Entity n e => EntityLike e

data NodeState n 
  = NodeState [(Bounds2 n, Node n)] 
  | LeafState [(Bounds2 n, EntityLike n)]

percentIncrease :: 
  (Ord n, Fractional n) => Bounds2 n -> Bounds2 n -> n
percentIncrease current new =
  area (union current new) / area current

data Compare a b = Compare a b

getItem :: Compare a b -> a
getItem (Compare a _) = a

instance Eq b => Eq (Compare a b) where
  Compare _ b1 == Compare _ b2 = b1 == b2

instance Ord b => Ord (Compare a b) where
  compare (Compare _ b1) (Compare _ b2) =
    compare b1 b2

minimumWith :: Ord b => (a -> b) -> [a] -> a
minimumWith f = 
  getItem . minimum . map (\x -> (Compare x (f x)))

-- Partial, fails on empty!
bestMatch :: 
  (Ord n, Fractional n) => 
    Bounds2 n -> [(Bounds2 n, a)] -> a
bestMatch test = 
  snd . minimumWith (percentIncrease test . fst)

nodeStep :: Node n -> NodeState n -> IO (NodeState n)
nodeStep (Node commands) state = do
  c <- takeMVar commands
  case c of
    Insert _els -> return ()
  return state

foreverWith :: (Monad m) => (a -> m a) -> a -> m ()
foreverWith step state =
  step state >>= foreverWith step

startNode :: NodeState n -> IO (Node n)
startNode initial = do
  cs <- newEmptyMVar
  let node = Node cs
  _nodeThread <- forkIO $ 
    foreverWith (nodeStep node) initial
  return $ Node cs

startEmpty :: IO (Node n)
startEmpty = startNode $ LeafState []


