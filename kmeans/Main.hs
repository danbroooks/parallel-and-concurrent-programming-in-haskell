module Main where

import Network.Wai.Handler.Warp
import Protolude
import Servant
import Types

type Kmeans = Api :<|> Raw

type Api = GetPage :<|> PostPage :<|> ResetPage

type App = ReaderT (MVar Page)

mapMVar :: MonadIO m => (a -> m a) -> MVar a -> m a
mapMVar f m = do
  read <- f =<< liftIO (takeMVar m)
  liftIO (putMVar m read)
  return read

retrievePage :: MonadIO m => App m Page
retrievePage = ask >>= liftIO . readMVar

type GetPage = "state" :> Get '[JSON] Page

getPage :: App Handler Page
getPage = retrievePage

type PostPage = "state" :> Post '[JSON] Page

postPage :: App Handler Page
postPage = ask >>= liftIO . mapMVar performStep

type ResetPage = "reset" :> Post '[JSON] Page

resetPage :: App Handler Page
resetPage = ask >>= liftIO . mapMVar (const initialPage)

everything :: Proxy Kmeans
everything = Proxy

handlers :: ServerT Kmeans (App Handler)
handlers = api :<|> serveDirectoryFileServer "./kmeans/assets"
  where
    api =
      getPage :<|> postPage :<|> resetPage

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
