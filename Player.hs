
{-# OPTIONS -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Player where

import Control.Applicative 
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
--import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC
import Linear.V2
import qualified Network.WebSockets as WS
import System.Random

import DistributedBVH

data ServerState = ServerState 
  { getRoot :: TVar (NodeState Double)
  , sendInsertPlayer :: MVar (Player)
  }

main :: IO ()
main = do
  root <- newTVarIO $ LeafState []
  recvInsertPlayer <- newEmptyMVar
  _spawnPlayerThread <- forkIO . forever $ do
    _newLeaf <- takeMVar recvInsertPlayer
    undefined
  let state = ServerState root recvInsertPlayer
  WS.runServer "0.0.0.0" 9160 $ application state

data Player = Player 
  { getPosition :: TVar (V2 Double) 
  , getRadius :: TVar Double
  , sendToClient :: MVar WS.DataMessage
  }

instance Entity Player Double where
  bounds player = 
    atomically $ do
      position <- readTVar . getPosition $ player
      radius <- readTVar . getRadius $ player
      let offset = V2 radius radius
          lo = position - offset
          hi = position + offset
      return $ Bounds2 lo hi
  update player _stepInput = do
    message <- atomically $ do
      position <- readTVar . getPosition $ player
      radius <- readTVar . getRadius $ player
      return . WS.Text . LBC.pack . show $ (position,radius)
    putMVar (sendToClient player) message
    Ok <$> bounds player
  
runPlayer :: ServerState -> WS.Connection -> IO ()
runPlayer _state conn = do
  putStrLn "Starting Player"
  pos <- V2 <$> randomRIO (-99, 99) <*> randomRIO (-99,99)
  rad <- randomRIO (5,20)
  dieConn :: MVar () <- newEmptyMVar
  toClient <- newEmptyMVar
  _player <- Player <$> newTVarIO pos <*> newTVarIO rad <*> return toClient
  _sendThreadId <- forkIO . forever $ do
    msg <- readMVar toClient
    WS.send conn $ WS.DataMessage msg
  takeMVar dieConn
  return ()

application :: ServerState -> WS.ServerApp
application state pending = do
  WS.acceptRequest pending >>= runPlayer state

