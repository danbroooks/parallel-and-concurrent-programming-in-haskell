module Kmeans
  ( KmeansState
  , initialState
  , randomState
  , iterateKmeans
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
  deriving (Show, Eq)

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

pointDistance :: Point -> Point -> (Int, Int)
pointDistance a b = (pointX a - pointX b, pointY a - pointY b)

pointCoords :: Point -> (Int, Int)
pointCoords Point{..} = (pointX, pointY)

movePoint :: (Int, Int) -> Point -> Point
movePoint (x', y') (Point r x y fill stroke) =
  Point r (x + x') (y + y') fill stroke

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
width = 1000

height :: Int
height = 700

centreX :: Int
centreX = width `div` 2

offsetX :: Int
offsetX = 3 * (centreX `div` 4)

centreY :: Int
centreY = height `div` 2

offsetY :: Int
offsetY = 3 * (centreY `div` 4)

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
      500

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
      pointFill $ minimumBy (compare `on` calcDistance pt) kmeansStateCentroids

    calcDistance a b = (xs ^ 2) + (ys ^ 2)
      where
        (xs, ys) = pointDistance a b

iterateKmeans :: KmeansState -> KmeansState
iterateKmeans KmeansState{..} = assignClusters $ KmeansState kmeansStateClusters centroids
  where
    centroids =
      (\c -> shiftCentroid (matchingClusters c) c) <$> kmeansStateCentroids

    matchingClusters c =
      filter (matchingCluster c) kmeansStateClusters

    matchingCluster centroid point =
      pointFill centroid == pointFill point

shiftCentroid :: [Point] -> Point -> Point
shiftCentroid cluster centroid = maybe centroid (performShift . calculateShift . pointCoords) firstPoint
  where
    firstPoint =
      listToMaybe cluster

    performShift shift =
      movePoint shift centroid

    calculateShift initialCoords =
      reduceTuple 2 . centroidDistance . reduceTuple (length cluster) . foldr takeAverage (0, 0) $ cluster

    takeAverage point acc =
      sumTuples acc . pointCoords $ point

    centroidDistance avg =
      subTuples avg (pointCoords centroid)

    reduceTuple n (x, y) =
      (x `div` n, y `div` n)

    sumTuples (ax, ay) (bx, by) =
      (ax + bx, ay + by)

    subTuples (ax, ay) (bx, by) =
      (ax - bx, ay - by)

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
