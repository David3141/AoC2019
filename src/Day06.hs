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


type Node = String
type OrbitTree = Map Node [Node]


part1 :: IO Int
part1 = countOrbits . parseOrbitTree <$> rawOrbits


part2 :: IO Int
part2 = do
    tree <- parseOrbitTree <$> rawOrbits

    let ancestorsA = reverse $ ancestorsOf "YOU" tree
    let ancestorsB = reverse $ ancestorsOf "SAN" tree

    return $ sumLevelsAfterLastCommonAncestor ancestorsA ancestorsB


rawOrbits :: IO [String]
rawOrbits = lines <$> (readFile =<< getDataFileName "inputs/day06.txt")


parseOrbitTree :: [Node] -> OrbitTree
parseOrbitTree =
    Map.fromListWith (++) . map ((\[a, b] -> (a, [b])) . splitOn ")")


countOrbits :: OrbitTree -> Int
countOrbits tree = countOrbits' 0 "COM"
  where
    countOrbits' :: Int -> Node -> Int
    countOrbits' currentOrbits node =
        currentOrbits + case Map.lookup node tree of
            Just orbitingNodes ->
                sum . map (countOrbits' (currentOrbits + 1)) $ orbitingNodes
            Nothing -> 0



ancestorsOf :: Node -> OrbitTree -> [Node]
ancestorsOf node tree = case ancestorOf node of
    Just ancestor -> ancestor : ancestorsOf ancestor tree
    Nothing       -> []
  where
    ancestorOf :: Node -> Maybe Node
    ancestorOf node = case take 1 . Map.keys . Map.filter (elem node) $ tree of
        []      -> Nothing
        (x : _) -> Just x


sumLevelsAfterLastCommonAncestor :: [Node] -> [Node] -> Int
sumLevelsAfterLastCommonAncestor (x : xs) (y : ys) = if x /= y
    then length (x : xs) + length (y : ys)
    else sumLevelsAfterLastCommonAncestor xs ys
