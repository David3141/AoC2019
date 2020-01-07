module Computer
  ( readIntCodes
  , withNounAndVerb
  , run
  , runNounVerb
  )
where

import           Paths_advent_of_code
import           Helpers                        ( readCommaSeparatedInts
                                                , setNth
                                                )

import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq )


readIntCodes :: FilePath -> IO (Seq Int)
readIntCodes filePath =
  Seq.fromList
    .   readCommaSeparatedInts
    <$> (readFile =<< getDataFileName filePath)


withNounAndVerb :: Int -> Int -> Seq Int -> Seq Int
withNounAndVerb noun verb = Seq.update 1 noun . Seq.update 2 verb


run :: Seq Int -> Int
run = (`at` 0) . run' 0
 where
  run' index seq | seq `at` index == 99 = seq
                | otherwise            = run' nextIndex updatedSeq
   where
    nextIndex  = index + 4
    updatedSeq = execOpcodeAt index seq


runNounVerb :: Int -> Int -> Seq Int -> Int
runNounVerb noun verb = run . withNounAndVerb noun verb


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
