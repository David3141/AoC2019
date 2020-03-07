module Day01
    ( part1
    , part2
    )
where

import           Helpers                        ( readInts )

part1 :: IO Int
part1 = sum . map calcFuel <$> moduleMasses

part2 :: IO Int
part2 = sum . map (calcFuelForFuel . calcFuel) <$> moduleMasses

calcFuel :: Int -> Int
calcFuel mass = mass `div` 3 - 2

calcFuelForFuel :: Int -> Int
calcFuelForFuel = sum . takeWhile (> 0) . iterate calcFuel

moduleMasses :: IO [Int]
moduleMasses = readInts "inputs/day01.txt"
