module Day02
  ( part1
  , part2
  )
where

import           Paths_advent_of_code
import           Helpers                        ( readCommaSeparatedInts
                                                , setNth
                                                )

import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq )


part1 :: IO Int
part1 = execProgram . prepareIntCodes 12 2 <$> readIntCodes


part2 :: IO Int
part2 = do
  intCodes <- readIntCodes

  let (noun, verb) = head
        [ (noun, verb)
        | noun <- [0 .. 99]
        , verb <- [0 .. 99]
        , execProgram (prepareIntCodes noun verb intCodes) == 19690720
        ]

  return $ 100 * noun + verb


readIntCodes :: IO (Seq Int)
readIntCodes =
  Seq.fromList
    .   readCommaSeparatedInts
    <$> (readFile =<< getDataFileName "inputs/day02.txt")


prepareIntCodes :: Int -> Int -> Seq Int -> Seq Int
prepareIntCodes noun verb = Seq.update 1 noun . Seq.update 2 verb


execProgram :: Seq Int -> Int
execProgram = (`at` 0) . run 0
 where
  run index seq | seq `at` index == 99 = seq
                | otherwise = run (index + 4) (execOpcodeAt index seq)


execOpcodeAt :: Int -> Seq Int -> Seq Int
execOpcodeAt index seq = Seq.update targetIndex newVal seq
 where
  opcode      = seq `at` index
  a           = seq `at` (seq `at` (index + 1))
  b           = seq `at` (seq `at` (index + 2))
  targetIndex = seq `at` (index + 3)
  newVal      = case opcode of
    1 -> a + b
    2 -> a * b


at :: Seq a -> Int -> a
at = Seq.index
