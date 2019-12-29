module Day02
  ( part1
  , part2
  )
where

import           Paths_advent_of_code
import           Helpers                        ( readCommaSeparatedInts
                                                , setNth
                                                )


part1 :: IO Int
part1 = execProgram . prepareIntCodes 12 2 <$> readIntCodes


part2 :: IO Int
part2 = do
  intCodes <- readIntCodes

  let checkNounVerb (noun, verb) =
        execProgram (prepareIntCodes noun verb intCodes) == 19690720

  let (noun, verb) =
        head $ filter checkNounVerb [ (i, j) | i <- [0 .. 99], j <- [0 .. 99] ]


  return $ 100 * noun + verb


readIntCodes :: IO [Int]
readIntCodes =
  readCommaSeparatedInts <$> (readFile =<< getDataFileName "inputs/day02.txt")


prepareIntCodes :: Int -> Int -> [Int] -> [Int]
prepareIntCodes noun verb = setNth 1 noun . setNth 2 verb


execProgram :: [Int] -> Int
execProgram = head . run 0
 where
  run index list | list !! index == 99 = list
                 | otherwise = run (index + 4) (execOpcodeAt index list)


execOpcodeAt :: Int -> [Int] -> [Int]
execOpcodeAt index list = setNth targetIndex newVal list
 where
  opcode      = list !! index
  a           = list !! (list !! (index + 1))
  b           = list !! (list !! (index + 2))
  targetIndex = list !! (index + 3)
  newVal      = case opcode of
    1 -> a + b
    2 -> a * b
