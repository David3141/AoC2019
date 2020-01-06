module Day04
  ( part1
  , part2
  )
where

import           Paths_advent_of_code
import           Data.List                      ( group )
import           Data.List.Split                ( splitOn )

part1 :: IO Int
part1 = length . filter (matchesPasswordConditon . show) <$> readRange


part2 :: IO Int
part2 = length . filter (matchesPasswordConditionExact . show) <$> readRange


readRange :: IO [Int]
readRange = do
  [start, end] <-
    map read . splitOn "-" <$> (readFile =<< getDataFileName "inputs/day04.txt")

  return [start .. end]


-- Not the most readable, I know, but I wanna learn/get used to applicatives.
matchesPasswordConditon :: (Eq a, Ord a) => [a] -> Bool
matchesPasswordConditon = (&&) <$> isSortedAsc <*> hasEqualPair


matchesPasswordConditionExact :: (Eq a, Ord a) => [a] -> Bool
matchesPasswordConditionExact = (&&) <$> isSortedAsc <*> hasEqualPairExact


hasEqualPair :: Eq a => [a] -> Bool
hasEqualPair = any ((>= 2) . length) . group


hasEqualPairExact :: Eq a => [a] -> Bool
hasEqualPairExact = any ((== 2) . length) . group


isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc [x           ] = True
isSortedAsc (x : y : rest) = (x <= y) && isSortedAsc (y : rest)
