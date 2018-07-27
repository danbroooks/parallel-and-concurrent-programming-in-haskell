module Pings
  ( RunningMode(..)
  , start
  ) where

import           Control.Distributed.Process
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Node (initRemoteTable)
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Static hiding (initRemoteTable)
import           Data.Binary
import qualified Data.Text as T
import           Prelude

data RunningMode
  = Master
  | Slave

data Msg
  = Ping ProcessId
  | Pong ProcessId
  deriving (Typeable, Generic)

instance Binary Msg

pingServer :: Process ()
pingServer = do
  Ping from <- expect
  say $ "Ping received from " <> show from
  send from . Pong =<< getSelfPid

remotable ['pingServer]

master :: [NodeId] -> Process ()
master peers = do
  ps <- forM peers $ \nid -> do
    say $ "Spawning on " <> show nid
    spawn nid $(mkStaticClosure 'pingServer)
  mypid <- getSelfPid
  forM_ ps $ \pid -> do
    say $ "Pinging " <> show pid
    send pid (Ping mypid)
  waitForPongs ps
  say "All pongs successfully received"
  terminate

waitForPongs :: [ProcessId] -> Process ()
waitForPongs [] = return ()
waitForPongs ps = do
  m <- expect
  case m of
    Pong p -> waitForPongs (filter (/= p) ps)
    _ -> say "MASTER received ping" >> terminate

start :: RunningMode -> Text -> Text -> IO ()
start mode host port = do
  backend <- initializeBackend (T.unpack host) (T.unpack port) (__remoteTable initRemoteTable)
  case mode of
    Master -> startMaster backend master
    Slave -> startSlave backend
