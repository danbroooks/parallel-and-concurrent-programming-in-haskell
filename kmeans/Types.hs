module Types where

import Data.Aeson.Types
import Kmeans
import Protolude

data Page = Page
  { pageTitle :: Text
  , pageState :: KmeansState
  }

instance ToJSON Page where
  toJSON Page{..} =
    object [ "title" .= pageTitle
           , "state" .= pageState
           ]

initialPage :: MonadIO m => m Page
initialPage = Page title <$> randomState
  where
    title =
      "kmeans - Parallel and Concurrent Programming in Haskell"
