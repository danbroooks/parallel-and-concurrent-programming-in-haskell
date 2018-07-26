module Prelude.List
  ( module Protolude.List
  , remove
  ) where

import Protolude
import Protolude.List

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove a (b:bs) | a == b    = remove a bs
                | otherwise = b : remove a bs
