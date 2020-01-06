module Day03
  ( part1
  , part2
  )
where

import Control.Monad (liftM2)
import Data.Maybe (fromMaybe)
import           Data.List                      ( foldl' )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq
                                                , Seq((:<|))
                                                , (><)
                                                , ViewR((:>))
                                                )
import           Data.Foldable                  ( toList )

import           Paths_advent_of_code
import           Helpers                        ( readCommaSeparatedStrings )

type PathInstruction = String -- direction and distance, like "R4" or "U12"
type Node = (Int, Int)
type Path = Seq Node

part1 :: IO Int
part1 = do
  (pathA, pathB) <- readPaths

  let intersections = intersectionSet pathA pathB
  let minimumDistance =
        minimum . Set.map (manhattanDistance (0, 0)) $ intersections

  return minimumDistance


part2 :: IO Int
part2 = do
  (pathA, pathB) <- readPaths

  let intersections = intersectionSet pathA pathB
  let minimumDistance =
        minimum . Set.map (wireDistance pathA pathB) $ intersections

  return minimumDistance


readPaths :: IO (Path, Path)
readPaths = do
  [firstPath, secondPath] <-
    lines <$> (readFile =<< getDataFileName "inputs/day03.txt")

  return (parsePath firstPath, parsePath secondPath)


parsePath :: String -> Path
parsePath = pathFromInstructions . readCommaSeparatedStrings


pathFromInstruction :: Node -> PathInstruction -> Path
pathFromInstruction (startX, startY) (direction : strDistance) = Seq.zip xs ys
 where
  distance = read strDistance
  xs       = case direction of
    'U' -> Seq.replicate distance startX
    'D' -> Seq.replicate distance startX
    'R' -> Seq.fromList [startX + 1 .. startX + distance]
    'L' -> Seq.fromList [startX - 1, startX - 2 .. startX - distance]
  ys = case direction of
    'U' -> Seq.fromList [startY + 1 .. startY + distance]
    'D' -> Seq.fromList [startY - 1, startY - 2 .. startY - distance]
    'R' -> Seq.replicate distance startY
    'L' -> Seq.replicate distance startY


pathFromInstructions :: [PathInstruction] -> Path
pathFromInstructions instructions = pathWithoutStartingNode
 where
  (_ :<| pathWithoutStartingNode) = foldl'
    (\acc curr -> acc >< pathFromInstruction (lastElem acc) curr)
    (Seq.singleton (0, 0))
    instructions


manhattanDistance :: Node -> Node -> Int
manhattanDistance (startX, startY) (x, y) =
  (abs x - abs startX) + (abs y - abs startY)


wireDistance :: Path -> Path -> Node -> Int
wireDistance pathA pathB intersection =
  -- Add 2 because we're working with two indices starting from 0
  2 + fromMaybe (error "Something went wrong") distance
 where
  distanceA = Seq.elemIndexL intersection pathA
  distanceB = Seq.elemIndexL intersection pathB
  distance = liftM2 (+) distanceA distanceB


lastElem :: Seq a -> a
lastElem seq = case Seq.viewr seq of
  Seq.EmptyR -> error "Empty Sequence, no last element!"
  as :> a    -> a


intersectionSet :: (Foldable t, Ord a) => t a -> t a -> Set a
intersectionSet a b =
  Set.intersection (Set.fromList $ toList a) (Set.fromList $ toList b)
