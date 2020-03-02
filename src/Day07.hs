module Day07
    ( part1
    , part2
    )
where

import           Data.List                      ( permutations )

import           Paths_advent_of_code
import qualified Computer.Computer             as C
import           Computer.IntCode               ( IntCode
                                                , readIntCode
                                                , at
                                                )

part1 :: IO Int
part1 = do
    intCode <- readIntCode "inputs/day07.txt"

    let signals =
            [ ampE
            | [phase1, phase2, phase3, phase4, phase5] <- permutations [0 .. 4]
            , let ampA = readOutput (phase1 : repeat 0) intCode
            , let ampB = readOutput (phase2 : repeat ampA) intCode
            , let ampC = readOutput (phase3 : repeat ampB) intCode
            , let ampD = readOutput (phase4 : repeat ampC) intCode
            , let ampE = readOutput (phase5 : repeat ampD) intCode
            ]

    return $ maximum signals


part2 :: IO Int
part2 = do
    intCode <- readIntCode "inputs/day07.txt"

    let signals =
            [ head ampE
            | [phase1, phase2, phase3, phase4, phase5] <- permutations [5 .. 9]
            , let   ampA = C.run (phase1 : 0 : reverse ampE) intCode
                    ampB = C.run (phase2 : reverse ampA) intCode
                    ampC = C.run (phase3 : reverse ampB) intCode
                    ampD = C.run (phase4 : reverse ampC) intCode
                    ampE = C.run (phase5 : reverse ampD) intCode
            ]

    return $ maximum signals



readOutput :: [Int] -> IntCode -> Int
readOutput inputs intCode = head $ C.run inputs intCode
