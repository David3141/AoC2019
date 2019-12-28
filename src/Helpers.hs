module Helpers
  ( readInts
  , readCommaSeparatedInts
  , setNth
  )
where

import           Data.List.Split                ( splitOn )

import           Paths_advent_of_code

readInts :: FilePath -> IO [Int]
readInts filePath =
  map read . lines <$> (readFile =<< getDataFileName filePath)

readCommaSeparatedInts :: FilePath -> IO [Int]
readCommaSeparatedInts filePath =
  map read . splitOn "," <$> (readFile =<< getDataFileName filePath)

setNth :: Int -> a -> [a] -> [a]
setNth ind new arr = set' ind 0 new arr
 where
  set' :: Int -> Int -> a -> [a] -> [a]
  set' _ _ _ [] = []
  set' ind curr new arr@(x : xs)
    | curr > ind  = arr
    | ind == curr = new : (set' ind (curr + 1) new xs)
    | otherwise   = x : (set' ind (curr + 1) new xs)
