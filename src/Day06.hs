module Day06
    ( part1
    , part2
    )
where

-- import           Data.List                      ( foldl' )
import           Data.List.Split                ( splitOn )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

import           Paths_advent_of_code


type OrbitList = Map String [String]


part1 :: IO Int
part1 = countOrbits . parseOrbitList <$> rawOrbits


part2 :: IO Int
part2 = return 0


rawOrbits :: IO [String]
rawOrbits = lines <$> (readFile =<< getDataFileName "inputs/day06.txt")


parseOrbitList :: [String] -> OrbitList
parseOrbitList =
    Map.fromListWith (++) . map ((\[a, b] -> (a, [b])) . splitOn ")")


countOrbits :: OrbitList -> Int
countOrbits orbitList = countOrbits' 0 "COM"
  where
    countOrbits' :: Int -> String -> Int
    countOrbits' currentOrbits node =
        currentOrbits + case Map.lookup node orbitList of
            Just orbitingNodes ->
                sum . map (countOrbits' (currentOrbits + 1)) $ orbitingNodes
            Nothing -> 0
