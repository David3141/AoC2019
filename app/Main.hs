module Main where

import           Control.Monad                  ( mapM_ )
import           System.Environment             ( getArgs )

import           Day01

main :: IO ()
main = do
  daysToRun <- map read <$> getArgs

  mapM_ run daysToRun

run :: Int -> IO ()
run 1 = runPretty 1 (Day01.part1, Day01.part2)

runPretty :: (Show a, Show b) => Int -> (IO a, IO b) -> IO ()
runPretty day (part1, part2) = do
  putStrLn $ "--- Day " ++ show day ++ " ---"

  putStr "Part 1: "
  part1 >>= print

  putStr "Part 2: "
  part2 >>= print

  putStrLn ""
