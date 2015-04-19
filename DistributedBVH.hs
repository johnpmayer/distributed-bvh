
{-# OPTIONS -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module DistributedBVH where

import Control.Applicative 
import Control.Concurrent
--import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (foldM)
import Data.List (maximumBy, partition, tails)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Linear.V2
import System.IO.Unsafe (unsafePerformIO) -- global node Id counter

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

data Command n = Insert (InsertParams n) (MVar (InsertResult n))

type InsertParams n = LeafChildren n

data InsertResult n
  = Inserted (Bounds2 n)
  | SplitNode (NodeState n) (NodeState n)

data Query 

data StepInput

nextNodeIdGlobal :: TVar Int
nextNodeIdGlobal = unsafePerformIO $ newTVarIO 0

genNodeId :: IO Int
genNodeId = atomically $ do
  nodeId <- readTVar nextNodeIdGlobal
  writeTVar nextNodeIdGlobal (nodeId + 1)
  return nodeId

data Node n = Node 
  { getNodeId :: Int 
  , sendToNode :: (MVar (Command n))
  }

data UpdateResult n = Ok (Bounds2 n) | Die

class Entity e n where
  bounds :: e -> IO (Bounds2 n)
  update :: e -> StepInput -> IO (UpdateResult n)
  -- Demonstrate extra optional behaviors
  defaultThing :: e -> Int -> IO ()
  defaultThing _ _ = return ()

data EntityLike n 
 = forall e. Entity e n => EntityLike e

type NodeChildren n = [Bounded2 n (Node n)] 
type LeafChildren n = [Bounded2 n (EntityLike n)]

newtype Height = Height Int
  deriving (Eq, Ord, Num)

data NodeState n 
  = NodeState Height (NodeChildren n)
  | LeafState (LeafChildren n)

unionAllOfBounds :: (Ord n) => [Bounded2 n a] -> Bounds2 n
unionAllOfBounds = foldl1 union . map fst

calcNodeBounds :: (Ord n) => NodeState n -> Bounds2 n
calcNodeBounds state = 
  case state of
    NodeState _ ns -> unionAllOfBounds ns
    LeafState ls -> unionAllOfBounds ls

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

nodeStep :: (Ord n, Fractional n) => 
  Command n -> NodeState n -> IO (Maybe (NodeState n))
nodeStep command state = do
  case command of
    Insert newEntities sendResult -> case state of
      NodeState h childNodes -> do
        results <- insertNode h childNodes newEntities
        case results of
          [] -> error "Broken Insert NonEmpty Invariant"
          [newChildren] -> do
            let result = Inserted $ unionAllOfBounds newChildren
            putMVar sendResult result
            return . Just $ NodeState h newChildren
          [split1, split2] -> do
            let result = SplitNode (NodeState h split1) (NodeState h split2)
            putMVar sendResult result
            return Nothing
          _ -> error "Broken Split > 2 Invariant"
      LeafState childEntities -> do
        case insertLeaf childEntities newEntities of
          [] -> error "Broken Insert NonEmpty Invariant"
          [newLeaves] -> do
            let result = Inserted $ unionAllOfBounds newLeaves
            putMVar sendResult result
            return . Just $ LeafState newLeaves
          [split1, split2] -> do
            let result = SplitNode (LeafState split1) (LeafState split2)
            putMVar sendResult result
            return Nothing
          _ -> error "Broken Split > 2 Invariant"

foreverUntil :: (Monad m) => (a -> m (Maybe a)) -> a -> m ()
foreverUntil step state = do
  newState <- step state
  case newState of
    Nothing -> return ()
    Just s -> foreverUntil step s

startNode :: (Ord n, Fractional n) => NodeState n -> IO (Node n)
startNode initial = do
  nodeId <- genNodeId
  commands <- newEmptyMVar
  let node = Node nodeId commands
  nodeThread <- forkIO $ foreverUntil (\state -> do
    cmd <- takeMVar commands
    nodeStep cmd state) initial
  putStrLn $ "Started Node: " ++ show nodeId ++ ": " ++ show nodeThread
  return node

startEmpty :: (Ord n, Fractional n) => IO (Node n)
startEmpty = startNode $ LeafState []

insertLeaf :: (Ord n, Num n) =>
  LeafChildren n -> LeafChildren n -> 
  [LeafChildren n]
insertLeaf leafChildren newLeaves = 
  let combined = leafChildren ++ newLeaves
      (split1, split2) = bestSplit combined
  in 
    if length combined <= maxNodeSize
    then [combined]
    else [split1, split2]

groupToMap :: Ord k => (a -> k) -> [a] -> Map k [a]
groupToMap fKey =
  let step x acc = Map.insertWith (++) (fKey x) [x] acc
  in foldr step Map.empty

addSplitNodes :: (Ord n, Fractional n) =>
  NodeState n -> NodeState n -> NodeChildren n -> IO (NodeChildren n)
addSplitNodes ns1 ns2 acc =
  let b1 = calcNodeBounds ns1
      b2 = calcNodeBounds ns2
  in do
    childNode1 <- startNode ns1
    childNode2 <- startNode ns2
    return $ (b1, childNode1) : (b2, childNode2) : acc

collectSubResults :: (Ord n, Fractional n) =>
  Height -> [(Node n, InsertResult n)] -> IO (NodeChildren n)
collectSubResults h = 
  let step :: (Ord n, Fractional n) =>
        NodeChildren n -> (Node n, InsertResult n) -> IO (NodeChildren n)
      step acc result =
        case result of
          _ | h < 1 -> error "Min non-leaf height is 1"
          (childNode, (Inserted b)) -> 
            return $ (b, childNode) : acc
          (_dead, (SplitNode l1@(LeafState _) l2@(LeafState _))) 
            | h == 1 ->
              addSplitNodes l1 l2 acc
          (_dead, (SplitNode n1@(NodeState h1 _) n2@(NodeState h2 _))) 
            | h == h1 + 1 && h == h2 + 1 -> 
              addSplitNodes n1 n2 acc
          _ -> error "Broken Collect Height Invariant"
  in foldM step []

insertNode :: (Ord n, Fractional n) =>
  Height -> NodeChildren n -> LeafChildren n -> 
  IO [NodeChildren n]
insertNode h nodeChildren newLeaves = 
  let chooseNode leaf = bestMatch (fst leaf) nodeChildren
      chosenNodes = map (\leaf -> (leaf, chooseNode leaf)) newLeaves
      subTasks = groupToMap (getNodeId . snd) chosenNodes
      spawnWork childNode = do
        let lns = Map.findWithDefault [] (getNodeId childNode) subTasks
            newLeaves' = fst <$> lns
        recvResult <- newEmptyMVar
        let work = Insert newLeaves' recvResult
        putMVar (sendToNode childNode) work
        (childNode,) <$> takeMVar recvResult
  in do
    results <- mapM spawnWork $ map snd nodeChildren
    newChildren <- collectSubResults h results
    let (split1, split2) = bestSplit newChildren
    return $ 
      if length newChildren <= maxNodeSize
      then [newChildren]
      else [split1, split2]

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

