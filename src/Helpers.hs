module Helpers
  ( readInts
  , readCommaSeparatedInts
  , readCommaSeparatedStrings
  , setNth
  )
where

import           Data.List.Split                ( splitOn )

import           Paths_advent_of_code

readInts :: FilePath -> IO [Int]
readInts filePath =
  map read . lines <$> (readFile =<< getDataFileName filePath)

-- readCommaSeparatedInts :: FilePath -> IO [Int]
-- readCommaSeparatedInts filePath =
--   map read . splitOn "," <$> (readFile =<< getDataFileName filePath)

readCommaSeparatedInts :: String -> [Int]
readCommaSeparatedInts = map read . splitOn ","

readCommaSeparatedStrings :: String -> [String]
readCommaSeparatedStrings = splitOn ","

setNth :: Int -> a -> [a] -> [a]
setNth = set' 0
 where
  set' :: Int -> Int -> a -> [a] -> [a]
  set' _ _ _ [] = []
  set' curr ind new arr@(x : xs)
    | curr > ind  = arr
    | ind == curr = new : set' (curr + 1) ind new xs
    | otherwise   = x : set' (curr + 1) ind new xs
