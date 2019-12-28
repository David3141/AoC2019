module Helpers where

import           Paths_advent_of_code

readInts :: FilePath -> IO [Int]
readInts filePath =
  map read . lines <$> (readFile =<< getDataFileName filePath)
