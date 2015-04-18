
{-# OPTIONS -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DistributedBVH where

import Control.Concurrent
import Control.Concurrent.STM
import Data.List (maximumBy, partition, tails)
import Linear.V2
import System.IO.Unsafe (unsafePerformIO) -- global Node Id counter

-- TODO for testing, make these tunable...

minNodeSize, maxNodeSize :: Int
minNodeSize = 4
maxNodeSize = 8

-- Bounds2

data Bounds2 a = Bounds2 !(V2 a) !(V2 a) deriving
  (Eq,Show)

type Bounded2 n a = (Bounds2 n, a)

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

data Command n = Insert (LeafChildren n)
data Query 
data StepInput

nextNodeIdGlobal :: TVar Int
nextNodeIdGlobal = unsafePerformIO $ newTVarIO 0

genNodeId :: IO Int
genNodeId = atomically $ do
  nodeId <- readTVar nextNodeIdGlobal
  writeTVar nextNodeIdGlobal (nodeId + 1)
  return nodeId

data Node n = Node Int (MVar (Command n)) 

data UpdateResult n = Ok (Bounds2 n) | Die

class Entity e n where
  bounds :: e -> IO (Bounds2 n)
  update :: e -> StepInput -> IO (UpdateResult n)
  -- Demonstrate extra optional behaviors
  defaultThing :: e -> Int -> IO ()
  defaultThing _ _ = return ()

data EntityLike n 
 = forall e. Entity n e => EntityLike e

type NodeChildren n = [Bounded2 n (Node n)] 
type LeafChildren n = [Bounded2 n (EntityLike n)]

data NodeState n 
  = NodeState (NodeChildren n)
  | LeafState (LeafChildren n)

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

bestMatch :: 
  (Ord n, Fractional n) => 
    Bounds2 n -> [(Bounds2 n, a)] -> a
bestMatch test = 
  snd . minimumWith (percentIncrease test . fst)

nodeStep :: Node n -> NodeState n -> IO (NodeState n)
nodeStep (Node _nodeId commands) state = do
  c <- takeMVar commands
  case c of
    Insert _els -> return ()
  return state

foreverWith :: (Monad m) => (a -> m a) -> a -> m ()
foreverWith step state =
  step state >>= foreverWith step

startNode :: NodeState n -> IO (Node n)
startNode initial = do
  nodeId <- genNodeId
  cs <- newEmptyMVar
  let node = Node nodeId cs
  _nodeThread <- forkIO $ 
    foreverWith (nodeStep node) initial
  return node

startEmpty :: IO (Node n)
startEmpty = startNode $ LeafState []

insertLeaf :: (Ord n, Num n) =>
  LeafChildren n -> LeafChildren n -> 
  [LeafChildren n]
insertLeaf leafChildren newLeaves = 
  let combined = leafChildren ++ newLeaves
      (split1, split2) = bestSplit combined
  in if length combined <= maxNodeSize
     then [combined]
     else [split1, split2]

insertNode :: 
  NodeChildren n -> LeafChildren n -> 
  IO [NodeChildren n]
insertNode = undefined

permutations2 :: [a] -> [(a,a)]
permutations2 list =
  concat $
  zipWith (zip . repeat) list $
  tails list

bestSplit :: (Ord n, Num n) =>
  [Bounded2 n a] -> ([Bounded2 n a],[Bounded2 n a])
bestSplit (children :: [Bounded2 n a]) = 
  let indexedChildren :: [(Bounded2 n a, Int)]
      indexedChildren = zip children [0..]
      pairsWithIndex = permutations2 indexedChildren
      worsePair (a,b) (c,d) =
        let fst2 = fst . fst
            ab = area $ union (fst2 a) (fst2 b)
            cd = area $ union (fst2 c) (fst2 d)
        in compare ab cd
      worst1, worst2 :: (Bounded2 n a, Int)
      (worst1, worst2) = maximumBy worsePair pairsWithIndex
      remaining :: [(Bounded2 n a, Int)]
      remaining = [ x | x <- indexedChildren, not $ elem (snd x) [snd worst1, snd worst2] ]
      group1, group2 :: [(Bounded2 n a, Int)]
      (group1, group2) = partition (\i -> worsePair (i,worst1) (i,worst2) == LT) remaining
      length1 = length group1
      length2 = length group2
  in (\(a,b) -> (map fst a, map fst b)) $
    if length1 + 1 < minNodeSize then
      let (group2A, group2B) = splitAt (minNodeSize - (length1 + 1)) group2
      in (worst1 : (group1 ++ group2A), worst2 : group2B)
    else if length2 + 1 < minNodeSize then
      let (group1A, group1B) = splitAt (minNodeSize - (length2 + 2)) group1
      in (worst1 : group1A, worst2 : (group1B ++ group2))
    else (worst1 : group1, worst2 : group2)

