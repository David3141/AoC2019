module Computer.IntCode
  ( IntCode
  , at
  , readIntCode
  , withNounAndVerb
  )
where

import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq )

import           Helpers                        ( readCommaSeparatedInts )
import           Paths_advent_of_code


type IntCode = Seq Int

readIntCode :: FilePath -> IO IntCode
readIntCode filePath =
  Seq.fromList
    .   readCommaSeparatedInts
    <$> (readFile =<< getDataFileName filePath)


withNounAndVerb :: Int -> Int -> IntCode -> IntCode
withNounAndVerb noun verb = Seq.update 1 noun . Seq.update 2 verb


at :: IntCode -> Int -> Int
at = Seq.index
