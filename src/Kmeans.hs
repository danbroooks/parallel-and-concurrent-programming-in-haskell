module Kmeans
  ( KmeansState
  , initialState
  ) where

import           Data.Aeson.Types
import           Protolude
import           Random

data Colour
  = None
  | White
  | Black
  | Red
  | Green
  | Blue
  | Orange

instance ToJSON Colour where
  toJSON None = "transparent"
  toJSON White = "#ffffff"
  toJSON Black = "#000000"
  toJSON Red = "#d62728"
  toJSON Green = "#2ca02c"
  toJSON Blue = "#1f77b4"
  toJSON Orange = "#ff7f0e"

data Point = Point
  { pointR :: Int
  , pointX :: Int
  , pointY :: Int
  , pointFill :: Colour
  , pointStroke :: Colour
  }

instance ToJSON Point where
  toJSON (Point r x y fill stroke) =
    object [ "r" .= r
           , "x" .= x
           , "y" .= y
           , "fill" .= fill
           , "stroke" .= stroke
           ]

data KmeansState = KmeansState
  { kmeansStateClusters :: [Point]
  , kmeansStateCentroids :: [Point]
  }

instance ToJSON KmeansState where
  toJSON KmeansState{..} =
    object [ "clusters" .= kmeansStateClusters
           , "centroids" .= kmeansStateCentroids
           ]

width :: Int
width = 400

height :: Int
height = 300

initialState :: MonadIO m => m KmeansState
initialState = runRandomT createState =<< seedFromTime
  where
    createState =
      KmeansState <$> initialClusters
                  <*> initialCentroids

initialClusters :: MonadIO m => RandomT m [Point]
initialClusters = sequence (replicate n . randomCluster =<< colours)
  where
    n = 20

    colours =
      [ Red
      , Green
      , Blue
      , Orange
      ]

    randomCluster colour =
      Point <$> pure 2
            <*> randomR (1, width)
            <*> randomR (1, height)
            <*> pure colour
            <*> pure None

initialCentroids :: MonadIO m => RandomT m [Point]
initialCentroids = sequence $ do
  replicate 3 randomCentroid

randomCentroid :: MonadIO m => RandomT m Point
randomCentroid = pure (Point 3 20 20 None Black)
