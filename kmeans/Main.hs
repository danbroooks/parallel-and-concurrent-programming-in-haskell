module Main where

import Network.Wai.Handler.Warp
import Protolude
import Servant
import Types

type Kmeans = Api :<|> Raw

type Api = GetPage :<|> ResetPage

type App = ReaderT (MVar Page)

alterPage :: MonadIO m => (Page -> Page) -> App m Page
alterPage f = ask >>= liftIO . \memory -> do
  read <- f <$> takeMVar memory
  putMVar memory read
  return read

retrievePage :: MonadIO m => App m Page
retrievePage = liftIO . readMVar =<< ask

type GetPage = "state" :> Get '[JSON] Page

getPage :: App Handler Page
getPage = retrievePage

type ResetPage = "reset" :> Post '[JSON] Page

resetPage :: App Handler Page
resetPage = alterPage =<< reset
  where
    reset =
      const <$> initialPage

everything :: Proxy Kmeans
everything = Proxy

handlers :: ServerT Kmeans (App Handler)
handlers = api :<|> serveDirectoryFileServer "./kmeans/assets"
  where
    api =
      getPage :<|> resetPage

server :: MVar Page -> Server Kmeans
server page = hoistServer everything (flip runReaderT page) handlers

main :: IO ()
main = logPort >> fetchPage >>= startServer
  where
    port = 8000

    fetchPage =
      initialPage >>= newMVar

    logPort =
      putText ("Running server on " <> show port)

    startServer =
      run port . serve everything . server
