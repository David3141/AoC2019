module Day08
    ( part1
    , part2
    )
where

import           Data.List                      ( minimumBy )
import           Data.Ord                       ( comparing )
import           Data.List.Split                ( chunksOf )
import Control.Monad (join)

import           Paths_advent_of_code


type Layer = [[Int]]

part1 :: IO Int
part1 = do
    layers <- parseImages

    let digits = join . minLayer $ layers
    let ones = countElem 1 digits
    let twos = countElem 2 digits

    return $ ones * twos


part2 :: IO Int
part2 = return 0


minLayer :: [Layer] -> Layer
minLayer = minimumBy (comparing countZerosInRows)
  where
    countZerosInRows = sum . map (countElem 0)


parseImages :: IO [[[Int]]]
parseImages = do
    (line : _) <- lines <$> (readFile =<< getDataFileName "inputs/day08.txt")

    return $ chunksOf 6 . chunksOf 25 . map (read . pure) $ line


countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (== x)
