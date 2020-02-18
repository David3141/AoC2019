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
            | phaseSetting <- permutations [0, 1, 2, 3, 4]
            , let ampA = readOutput (head phaseSetting : repeat 0) intCode
            , let ampB = readOutput (phaseSetting !! 1 : repeat ampA) intCode
            , let ampC = readOutput (phaseSetting !! 2 : repeat ampB) intCode
            , let ampD = readOutput (phaseSetting !! 3 : repeat ampC) intCode
            , let ampE = readOutput (phaseSetting !! 4 : repeat ampD) intCode
            ]

    return $ maximum signals


part2 :: IO Int
part2 = return 500
-- part2 = do
--     intCode <- readIntCode "inputs/day07.txt"

--     let signals =
--             [ ampE
--             | phaseSetting <- permutations [0, 1, 2, 3, 4]
--             , let ampA = readOutput (head phaseSetting : repeat 0) intCode
--             , let ampB = readOutput (phaseSetting !! 1 : repeat ampA) intCode
--             , let ampC = readOutput (phaseSetting !! 2 : repeat ampB) intCode
--             , let ampD = readOutput (phaseSetting !! 3 : repeat ampC) intCode
--             , let ampE = readOutput (phaseSetting !! 4 : repeat ampD) intCode
--             ]

--     return $ maximum signals



readOutput :: [Int] -> IntCode -> Int
readOutput inputs intCode = head $ C.run inputs intCode
