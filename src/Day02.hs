module Day02
  ( part1
  , part2
  )
where

import           Helpers                        ( readCommaSeparatedInts
                                                , setNth
                                                )


part1 :: IO Int
part1 = head . runPrograms 0 . setNth 2 2 . setNth 1 12 <$> programNumbers
 where
  runPrograms :: Int -> [Int] -> [Int]
  runPrograms index list
    | list !! index == 99 = list
    | otherwise           = runPrograms (index + 4) (runProgram index list)


part2 :: IO Int
part2 = return 0


programNumbers :: IO [Int]
programNumbers = readCommaSeparatedInts "inputs/day02.txt"


runProgram :: Int -> [Int] -> [Int]
runProgram index list = setNth targetIndex newVal list
 where
  program     = list !! index
  a           = list !! (list !! (index + 1))
  b           = list !! (list !! (index + 2))
  targetIndex = list !! (index + 3)
  newVal      = case program of
    1 -> a + b
    2 -> a * b
