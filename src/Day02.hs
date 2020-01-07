module Day02
  ( part1
  , part2
  )
where

import           Paths_advent_of_code
import qualified Computer                      as C
import           Helpers                        ( readCommaSeparatedInts
                                                , setNth
                                                )

import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq )


part1 :: IO Int
part1 =
  C.runNounVerb 12 2 <$> C.readIntCodes "inputs/day02.txt"


part2 :: IO Int
part2 = do
  intCodes <- C.readIntCodes "inputs/day02.txt"

  let (noun, verb) = head
        [ (noun, verb)
        | noun <- [0 .. 99]
        , verb <- [0 .. 99]
        , C.runNounVerb noun verb intCodes == 19690720
        ]

  return $ 100 * noun + verb
