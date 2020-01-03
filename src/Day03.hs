module Day03
  ( part1
  , part2
  )
where

import           Paths_advent_of_code

import qualified Data.Set                      as Set
import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq
                                                , Seq((:<|))
                                                , (><)
                                                , ViewR((:>))
                                                )
import           Data.Foldable                  ( toList )

import           Helpers                        ( readCommaSeparatedStrings )

type PathInstruction = String -- direction and distance, like "R4" or "U12"
type Node = (Int, Int)
type Path = [Node]


part1 :: IO Int
part1 = do
  print "-"

  (instructionsA, instructionsB) <- readPathInstructions

  -- let pathA         = pathFromInstructions instructionsA
  -- let pathB         = pathFromInstructions instructionsB

  -- print $ "Set 1, length " ++ show (Set.size pathA)
  -- print $ Set.take 5 pathA

  -- print $ "Set 2, length " ++ show (Set.size pathB)
  -- print $ Set.take 5 pathB

  -- let intersections = Set.intersection pathA pathB

  -- print intersections

  -- let minimumDistance =
  --       minimum . Set.map (manhattanDistance (0, 0)) $ intersections

  -- return minimumDistance

  -- Due to how I implemented this, the first two elements are the root node.
  let (_ :<| _ :<| pathA2) = pathFromInstructions2 instructionsA
  let (_ :<| _ :<| pathB2) = pathFromInstructions2 instructionsB

  let intersections2 = Set.intersection (Set.fromList $ toList pathA2)
                                        (Set.fromList $ toList pathB2)

  let minimumDistance2 =
        minimum . Set.map (manhattanDistance (0, 0)) $ intersections2

  return minimumDistance2


part2 :: IO Int
part2 = return 0


readPathInstructions :: IO ([PathInstruction], [PathInstruction])
readPathInstructions = do
  [firstPath, secondPath] <-
    map readCommaSeparatedStrings
    .   lines
    <$> (readFile =<< getDataFileName "inputs/day03.txt")

  return (firstPath, secondPath)


-- pathFromInstruction :: Node -> PathInstruction -> Path
-- pathFromInstruction (startX, startY) (direction : strDistance) = zip xs ys
--  where
--   distance = read strDistance
--   xs       = case direction of
--     'U' -> repeat startX
--     'D' -> repeat startX
--     'R' -> [startX .. startX + distance]
--     'L' -> [startX, pred startX .. startX - distance]
--   ys = case direction of
--     'U' -> [startY .. startY + distance]
--     'D' -> [startY, pred startY .. startY - distance]
--     'R' -> repeat startY
--     'L' -> repeat startY

pathFromInstruction2 :: Node -> PathInstruction -> Seq Node
pathFromInstruction2 (startX, startY) (direction : strDistance) = Seq.zip xs ys
 where
  distance = read strDistance
  xs       = case direction of
    'U' -> Seq.replicate distance startX
    'D' -> Seq.replicate distance startX
    'R' -> Seq.fromFunction distance (+ succ startX)
    'L' -> Seq.fromFunction distance ((-) $ pred startX)
    -- 'R' -> Seq.fromList [startX + 1 .. startX + distance]
    -- 'L' -> Seq.fromList [startX - 1, startX - 2 .. startX - distance]
  ys = case direction of
    'U' -> Seq.fromFunction distance (+ succ startY)
    'D' -> Seq.fromFunction distance ((-) $ pred startY)
    -- 'U' -> Seq.fromList [startY + 1 .. startY + distance]
    -- 'D' -> Seq.fromList [startY - 1, startY - 2 .. startY - distance]
    'R' -> Seq.replicate distance startY
    'L' -> Seq.replicate distance startY


pathFromInstructions2 :: [PathInstruction] -> Seq Node
pathFromInstructions2 = foldl
  (\acc curr -> acc >< pathFromInstruction2 (lastThing acc) curr)
  (Seq.singleton (0, 0))

-- pathFromInstructions :: [PathInstruction] -> Set.Set Node
-- pathFromInstructions = Set.delete (0, 0) . Set.fromList . foldl
--   (\acc curr -> acc ++ pathFromInstruction (last acc) curr)
--   [(0, 0)]


manhattanDistance :: Node -> Node -> Int
manhattanDistance (startX, startY) (x, y) =
  (abs x - abs startX) + (abs y - abs startY)


lastThing :: Seq a -> a
lastThing seq = case Seq.viewr seq of
  Seq.EmptyR -> error "no end"
  as :> a    -> a
