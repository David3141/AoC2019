module Day05
    ( part1
    , part2
    )
where

import           Data.List                      ( group )
import           Data.List.Split                ( splitOn )
import           Data.Sequence                  ( fromList
                                                , Seq((:|>))
                                                )

import           Paths_advent_of_code
import qualified Computer.Computer             as C
import           Computer.IntCode               ( IntCode
                                                , readIntCode
                                                , at
                                                )


part1 :: IO Int
part1 =
    readDiagnosticCode . C.runWithInput 1 <$> readIntCode "inputs/day05.txt"


part2 :: IO Int
part2 = return 0


readDiagnosticCode :: (IntCode, [Int]) -> Int
readDiagnosticCode = head . snd
