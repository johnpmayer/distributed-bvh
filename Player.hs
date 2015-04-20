
{-# OPTIONS -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Player where

import Control.Applicative 
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Suspend
import Control.Concurrent.Timer
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LBC
import Linear.V2
import qualified Network.WebSockets as WS
import System.Random

import DistributedBVH
import Logging

data ServerState = ServerState 
  { getRoot :: MVar (Height, Node Double)
  , sendInsertPlayer :: MVar (Player)
  }

spawnNewPlayer :: ServerState -> IO ()
spawnNewPlayer state = do
  newPlayer <- takeMVar (sendInsertPlayer state)
  oldRoot@(h, rootNode) <- takeMVar (getRoot state)
  recvResult <- newEmptyMVar
  playerBounds <- bounds newPlayer
  let newEntity :: EntityLike Double = EntityLike newPlayer
  let command = Insert [(playerBounds, newEntity)] recvResult
  putMVar (sendToNode rootNode) command
  result <- takeMVar recvResult
  case result of
    Inserted _newBounds -> 
      putMVar (getRoot state) oldRoot
    SplitNode split1 split2 -> do
      newRootChildren <- addSplitNodes split1 split2 []
      let newH = h + 1
      newRoot <- startNode $ NodeState newH newRootChildren
      putMVar (getRoot state) (newH, newRoot)

updateWorld :: ServerState -> IO ()
updateWorld state = do
  root@(_, rootNode) <- takeMVar (getRoot state)
  logInfo "Updating World"
  recvFinished <- newEmptyMVar
  let command = Update recvFinished
  putMVar (sendToNode rootNode) command
  takeMVar recvFinished
  putMVar (getRoot state) root

main :: IO ()
main = do
  root <- (0,) <$> startEmpty >>= newMVar
  recvInsertPlayer <- newEmptyMVar
  let state = ServerState root recvInsertPlayer
  _spawnNewPlayersThread <- forkIO . forever $ spawnNewPlayer state
  _updateTimer <- repeatedTimer (updateWorld state) (msDelay 100)
  WS.runServer "0.0.0.0" 9160 $ application state

data Player = Player 
  { getPosition :: TVar (V2 Double) 
  , getRadius :: TVar Double
  , sendToClient :: MVar WS.DataMessage
  }

instance Entity Player where
  type N Player = Double
  bounds player = 
    atomically $ do
      position <- readTVar . getPosition $ player
      radius <- readTVar . getRadius $ player
      let offset = V2 radius radius
          lo = position - offset
          hi = position + offset
      return $ Bounds2 lo hi
  update player _stepInput = do
    logInfo "Updating Player"
    message <- atomically $ do
      position <- readTVar . getPosition $ player
      radius <- readTVar . getRadius $ player
      return . WS.Text . LBC.pack . show $ (position,radius)
    putMVar (sendToClient player) message
    Ok <$> bounds player
  
runPlayer :: ServerState -> WS.Connection -> IO ()
runPlayer state conn = do
  logInfo "Starting Player"
  pos <- V2 <$> randomRIO (-99, 99) <*> randomRIO (-99,99)
  rad <- randomRIO (5,20)
  dieConn :: MVar () <- newEmptyMVar
  toClient <- newEmptyMVar
  player <- Player <$> newTVarIO pos <*> newTVarIO rad <*> return toClient
  putMVar (sendInsertPlayer state) player
  _sendThreadId <- forkIO . forever $ do
    msg <- takeMVar toClient
    logInfo "Sending to player"
    WS.send conn $ WS.DataMessage msg
  takeMVar dieConn
  return ()

application :: ServerState -> WS.ServerApp
application state pending = do
  WS.acceptRequest pending >>= runPlayer state

