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
  say $ "Ping received from " <> (show from)
  send from . Pong =<< getSelfPid

remotable ['pingServer]

master :: Process ()
master = do
  node <- getSelfNode
  say $ "Spawning on " <> show node
  pid <- spawn node $(mkStaticClosure 'pingServer)
  say $ "Sending ping to " <> show pid
  send pid . Ping =<< getSelfPid
  Pong _ <- expect
  say "Pong"
  terminate

start :: RunningMode -> Text -> Text -> IO ()
start mode host port = do
  backend <- initializeBackend (T.unpack host) (T.unpack port) (__remoteTable initRemoteTable)
  case mode of
    Master -> startMaster backend (\_ -> master)
    Slave -> startSlave backend
