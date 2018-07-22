module Kmeans
  ( KmeansState
  , initialState
  , randomState
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
width = 800

height :: Int
height = 600

centreX :: Int
centreX = width `div` 2

offsetX :: Int
offsetX = 2 * (centreX `div` 3)

centreY :: Int
centreY = height `div` 2

offsetY :: Int
offsetY = 2 * (centreY `div` 3)

initialState :: MonadIO m => Int -> Int -> m KmeansState
initialState nClusters nCentroids = runRandomT createState =<< seedFromTime
  where
    createState =
      KmeansState <$> initialClusters nClusters
                  <*> replicateM nCentroids randomCentroid

randomState :: MonadIO m => m KmeansState
randomState = runRandomT createState =<< seedFromTime
  where
    createState = do
      clusters <- randomR (2, 5)
      centroids <- randomR (2, 5)
      initialState clusters centroids

initialClusters :: MonadIO m => Int -> RandomT m [Point]
initialClusters numClusters = join <$> replicateM numClusters randomCluster
  where
    totalPoints =
      300

    perCluster =
      totalPoints `div` numClusters

    randomCluster =
      generateCluster (perCluster - 1) =<< randomClusterOrigin

generateCluster :: MonadIO m => Int -> Point -> RandomT m [Point]
generateCluster numPoints pt = generate
  where
    generate =
      if numPoints == 0 then pure [pt]
                        else (:) <$> groupedPoint <*> recurr

    originX =
      pointX pt

    originY =
      pointY pt

    spreadX =
      width `div` numPoints * 3

    spreadY =
      height `div` numPoints * 3

    xR =
      ( max 1 $ originX - spreadX
      , min width $ originX + spreadX
      )

    yR =
      ( max 1 $ originY - spreadY
      , min height $ originY + spreadY
      )

    groupedPoint =
      Point <$> pure 2
            <*> randomR xR
            <*> randomR yR
            <*> pure Red
            <*> pure None

    recurr =
      generateCluster (numPoints - 1) pt

randomClusterOrigin :: MonadIO m => RandomT m Point
randomClusterOrigin = generate
  where
    generate =
      Point <$> pure 2
            <*> randomR (centreX - offsetX, centreX + offsetX)
            <*> randomR (centreY - offsetY, centreY + offsetY)
            <*> pure Red
            <*> pure None

randomCentroid :: MonadIO m => RandomT m Point
randomCentroid =
  Point <$> pure 5
        <*> randomR (centreX - offsetX, centreX + offsetX)
        <*> randomR (centreY - offsetY, centreY + offsetY)
        <*> pure White
        <*> pure Black
