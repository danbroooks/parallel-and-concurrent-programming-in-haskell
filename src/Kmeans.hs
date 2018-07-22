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
  | Grey
  | Red
  | Green
  | Orange
  | Blue
  | Purple
  | Teal

instance ToJSON Colour where
  toJSON None = "transparent"
  toJSON White = "#ecf0f1"
  toJSON Black = "#2c3e50"
  toJSON Grey = "#34495e"
  toJSON Red = "#c13f2b"
  toJSON Green = "#55af61"
  toJSON Orange = "#f39c28"
  toJSON Blue = "#2980b9"
  toJSON Purple = "#8f50ad"
  toJSON Teal = "#49a185"

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

pointDistance :: Point -> Point -> Int
pointDistance a b = xs + ys
  where
    xs =
      (pointX a - pointX b) ^ 2

    ys =
      (pointY a - pointY b) ^ 2

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
      assignClusters <$> generateState

    generateState =
      KmeansState <$> initialClusters nClusters
                  <*> initialCentroids

    initialCentroids =
      mapM randomCentroid $ take nCentroids centroidColours

    centroidColours =
      [ Red
      , Green
      , Orange
      , Blue
      , Purple
      , Teal
      ]

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

assignClusters :: KmeansState -> KmeansState
assignClusters KmeansState{..} = KmeansState clusters kmeansStateCentroids
  where
    clusters =
      (\pt -> assignColour (nearestColour pt) pt) <$> kmeansStateClusters

    assignColour fill (Point r x y _ stroke) =
      Point r x y fill stroke

    nearestColour pt =
      pointFill $ minimumBy (compare `on` pointDistance pt) kmeansStateCentroids

randomClusterOrigin :: MonadIO m => RandomT m Point
randomClusterOrigin = generate
  where
    generate =
      Point <$> pure 2
            <*> randomR (centreX - offsetX, centreX + offsetX)
            <*> randomR (centreY - offsetY, centreY + offsetY)
            <*> pure Red
            <*> pure None

randomCentroid :: MonadIO m => Colour -> RandomT m Point
randomCentroid colour =
  Point <$> pure 5
        <*> randomR (centreX - offsetX, centreX + offsetX)
        <*> randomR (centreY - offsetY, centreY + offsetY)
        <*> pure colour
        <*> pure Black
