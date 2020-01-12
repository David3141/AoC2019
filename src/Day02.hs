module Day02
  ( part1
  , part2
  )
where

import           Paths_advent_of_code
import qualified Computer.Computer             as C
import           Computer.IntCode               ( IntCode
                                                , readIntCode
                                                , at
                                                )
import           Helpers                        ( readCommaSeparatedInts )

import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq )


part1 :: IO Int
part1 = readResult . C.runNounVerb 12 2 <$> readIntCode "inputs/day02.txt"


part2 :: IO Int
part2 = do
  intCodes <- readIntCode "inputs/day02.txt"

  let (noun, verb) = head
        [ (noun, verb)
        | noun <- [0 .. 99]
        , verb <- [0 .. 99]
        , (== 19690720) . readResult . C.runNounVerb noun verb $ intCodes
        ]

  return $ 100 * noun + verb


readResult :: (IntCode, [Int]) -> Int
readResult = (`at` 0) . fst
