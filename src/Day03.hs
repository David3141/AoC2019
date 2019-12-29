module Day03
  ( part1
  , part2
  )
where

import           Paths_advent_of_code

import qualified Data.Set                      as Set

import           Helpers                        ( readCommaSeparatedStrings )

type PathInstruction = String -- direction and distance, like "R4" or "U12"
type Node = (Int, Int)
type Path = [Node]


part1 :: IO Int
part1 = do
  (instructionsA, instructionsB) <- readPathInstructions

  let pathA         = pathFromInstructions instructionsA
  let pathB         = pathFromInstructions instructionsB

  let intersections = Set.intersection pathA pathB
  let minimumDistance =
        minimum . Set.map (manhattanDistance (0, 0)) $ intersections

  return minimumDistance


part2 :: IO Int
part2 = return 0


readPathInstructions :: IO ([PathInstruction], [PathInstruction])
readPathInstructions = do
  [firstPath, secondPath] <-
    map readCommaSeparatedStrings
    .   lines
    <$> (readFile =<< getDataFileName "inputs/day03.txt")

  return (firstPath, secondPath)


pathFromInstruction :: Node -> PathInstruction -> Path
pathFromInstruction (startX, startY) (direction : strDistance) = zip xs ys
 where
  distance = read strDistance
  xs       = case direction of
    'U' -> repeat startX
    'D' -> repeat startX
    'R' -> [startX .. startX + distance]
    'L' -> [startX, pred startX .. startX - distance]
  ys = case direction of
    'U' -> [startY .. startY + distance]
    'D' -> [startY, pred startY .. startY - distance]
    'R' -> repeat startY
    'L' -> repeat startY


pathFromInstructions :: [PathInstruction] -> Set.Set Node
pathFromInstructions = Set.delete (0, 0) . Set.fromList . foldl
  (\acc curr -> acc ++ pathFromInstruction (last acc) curr)
  [(0, 0)]


manhattanDistance :: Node -> Node -> Int
manhattanDistance (startX, startY) (x, y) =
  (abs x - abs startX) + (abs y - abs startY)
