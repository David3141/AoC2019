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
                                                , withNounAndVerb
                                                )
import           Helpers                        ( readCommaSeparatedInts )

import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq )
import           Control.Monad.Trans.State


part1 :: IO Int
part1 = runNounVerb 12 2 <$> readIntCode "inputs/day02.txt"


part2 :: IO Int
part2 = do
    intCode <- readIntCode "inputs/day02.txt"

    let (noun, verb) = head
            [ (noun, verb)
            | noun <- [0 .. 99]
            , verb <- [0 .. 99]
            , let result = runNounVerb noun verb intCode
            , result == 19690720
            ]

    return $ 100 * noun + verb


runNounVerb :: Int -> Int -> IntCode -> Int
runNounVerb noun verb = (`at` 0) . C.runForIntCode . withNounAndVerb noun verb
