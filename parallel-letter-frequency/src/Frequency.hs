module Frequency (frequency) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Char (isAlpha)

import Control.Parallel.Strategies

type CharCounts = M.Map Char Int

-- doesn't actually run in parallel
-- I'm going to look at other solutions
frequency :: Int -> [T.Text] -> CharCounts
frequency nWorkers =
  mergeCounts
  . parMap rpar (countText . filterText)
  . T.chunksOf nWorkers
  . T.concat

mergeCounts :: [CharCounts] -> CharCounts
mergeCounts = M.unionsWith (+)

countText :: T.Text -> CharCounts
countText = T.foldl' (\m c -> M.insertWith (+) c 1 m) M.empty

filterText :: T.Text -> T.Text
filterText =  T.toLower . T.filter isAlpha
