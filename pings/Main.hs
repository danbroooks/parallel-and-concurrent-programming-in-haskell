module Main where

import qualified Pings
import           Pings (RunningMode(..))
import           Prelude

main :: IO ()
main = getRunningMode >>= maybe handleInvalidMode startServer
  where
    startServer mode = do
      (host, port) <- getHostname
      Pings.start mode host port

    handleInvalidMode =
      putText "Invalid mode provided, please specify `master` or `slave`"

getRunningMode :: MonadIO m => m (Maybe RunningMode)
getRunningMode = (maybe Nothing mapArgToMode) <$> getArg 0
  where
    mapArgToMode "master" = Just Master
    mapArgToMode "slave" = Just Slave
    mapArgToMode _ = Nothing

getHostname :: MonadIO m => m (Text, Text)
getHostname = readFromArgs . drop 1 <$> getArgs
  where
    readFromArgs [host, port] = (host, port)
    readFromArgs [port] = (defaultHost, port)
    readFromArgs _ = (defaultHost, defaultPort)

    defaultHost = "localhost"

    defaultPort = "44444"
