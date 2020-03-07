module Main where

import           Control.Monad                  ( mapM_ )
import           System.Environment             ( getArgs )
import           System.TimeIt                  ( timeIt )

import           Day01
import           Day02
import           Day03
import           Day04
import           Day05
import           Day06
import           Day07
import           Day08

main :: IO ()
main = do
    daysToRun <- map read <$> getArgs

    mapM_ run daysToRun

run :: Int -> IO ()
run 1 = runPretty 1 (Day01.part1, Day01.part2)
run 2 = runPretty 2 (Day02.part1, Day02.part2)
run 3 = runPretty 3 (Day03.part1, Day03.part2)
run 4 = runPretty 4 (Day04.part1, Day04.part2)
run 5 = runPretty 5 (Day05.part1, Day05.part2)
run 6 = runPretty 6 (Day06.part1, Day06.part2)
run 7 = runPretty 7 (Day07.part1, Day07.part2)
run 8 = runPretty 8 (Day08.part1, Day08.part2)

runPretty :: (Show a, Show b) => Int -> (IO a, IO b) -> IO ()
runPretty day (part1, part2) = do
    putStrLn $ "--- Day " ++ show day ++ " ---"

    putStr "Part 1:     "
    timeIt (print =<< part1)

    putStr "\nPart 2:     "
    timeIt (print =<< part2)

    putStrLn ""
