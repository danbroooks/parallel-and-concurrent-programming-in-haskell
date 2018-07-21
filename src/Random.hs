module Random
  ( RandomT
  , seedFromTime
  , runRandomT
  , random
  , randomR
  , takeRandom
  , shuffle
  ) where

import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Protolude
import qualified System.Random as R
import           System.Random (Random, StdGen, mkStdGen)

type RandomT = StateT StdGen

seedFromTime :: MonadIO m => m StdGen
seedFromTime = liftIO $ mkStdGen . round <$> getPOSIXTime

runRandomT :: MonadIO m => RandomT m a -> StdGen -> m a
runRandomT = evalStateT

random :: (MonadIO m, Random a) => RandomT m a
random = do
  (n, rand) <- R.random <$> get
  _ <- put rand
  pure n

randomR :: (MonadIO m, Random a) => (a, a) -> RandomT m a
randomR (from, to) = do
  (n, rand) <- R.randomR (from, to) <$> get
  _ <- put rand
  pure n

takeRandom :: MonadIO m => [a] -> RandomT m (Maybe a)
takeRandom as = listToMaybe <$> shuffle as

shuffle :: MonadIO m => [a] -> RandomT m [a]
shuffle [] = pure []
shuffle as = do
  n <- randomR (0, length as - 1)
  first <- shuffle (take n as)
  second <- shuffle (drop n as)
  pure $ second <> first
