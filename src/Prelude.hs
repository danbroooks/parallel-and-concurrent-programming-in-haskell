module Prelude
  ( module Protolude
  , getArgs
  , getArg
  ) where

import qualified Data.Text as T
import qualified Protolude as Base
import           Protolude hiding (getArgs)

getArgs :: MonadIO m => m [Text]
getArgs = fmap T.pack <$> liftIO Base.getArgs

getArg :: MonadIO m => Int -> m (Maybe Text)
getArg n = head . drop n <$> getArgs
