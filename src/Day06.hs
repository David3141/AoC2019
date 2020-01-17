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

part1 :: IO Int
part1 = do
    orbits <- parseOrbits <$> readOrbits

    print orbits
    print $ countOrbits "COM" orbits 0 0


    return 0

part2 :: IO Int
part2 = return 0


readOrbits :: IO [String]
readOrbits = lines <$> (readFile =<< getDataFileName "inputs/day06.txt")
-- readOrbits = return testOrbits


parseOrbits :: [String] -> Map String [String]
parseOrbits = Map.fromListWith (++) . map ((\[a, b] -> (a, [b])) . splitOn ")")


countOrbits :: String -> Map String [String] -> Int -> Int -> Int
countOrbits nodeName orbits currentLevel currentOrbits =
    currentOrbits + case Map.lookup nodeName orbits of
        Just orbitingNodes ->
            sum
                . map
                      (\node -> countOrbits node
                                            orbits
                                            (currentLevel + 1)
                                            (currentOrbits + 1)
                      )
                $ orbitingNodes
        Nothing -> 0



testOrbits =
    [ "COM)B"
    , "B)C"
    , "C)D"
    , "D)E"
    , "E)F"
    , "B)G"
    , "G)H"
    , "D)I"
    , "E)J"
    , "J)K"
    , "K)L"
    ]
