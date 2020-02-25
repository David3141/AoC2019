module Day05
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


part1 :: IO Int
part1 =
    readDiagnosticCode . runWithSingleInput 1 <$> readIntCode "inputs/day05.txt"


part2 :: IO Int
part2 =
    readDiagnosticCode . runWithSingleInput 5 <$> readIntCode "inputs/day05.txt"


readDiagnosticCode :: [Int] -> Int
readDiagnosticCode = last


runWithSingleInput :: Int -> IntCode -> [Int]
runWithSingleInput input = C.run (repeat input)
