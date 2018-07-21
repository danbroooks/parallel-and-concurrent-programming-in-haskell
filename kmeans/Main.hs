module Main where

import Data.Aeson.Types
import Kmeans
import Network.Wai.Handler.Warp
import Protolude
import Servant

type Kmeans = Api :<|> Raw

type Api = GetState

type GetState = "state" :> Get '[JSON] Page

data Page = Page
  { pageTitle :: Text
  , pageState :: KmeansState
  }

instance ToJSON Page where
  toJSON Page{..} =
    object [ "title" .= pageTitle
           , "state" .= pageState
           ]

type App = ReaderT (MVar Page)

alterPage :: MonadIO m => (Page -> Page) -> App m Page
alterPage f = ask >>= liftIO . \memory -> do
  read <- f <$> takeMVar memory
  putMVar memory read
  return read

retrievePage :: MonadIO m => App m Page
retrievePage = liftIO . readMVar =<< ask

initialPage :: MonadIO m => m Page
initialPage = Page title <$> initialState
  where
    title =
      "kmeans - Parallel and Concurrent Programming in Haskell"

getPage :: App Handler Page
getPage = retrievePage

everything :: Proxy Kmeans
everything = Proxy

handlers :: ServerT Kmeans (App Handler)
handlers = getPage :<|> serveDirectoryFileServer "./kmeans/assets"

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
