module Computer.Computer
  ( run
  , runNounVerb
  )
where

import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq )

import           Paths_advent_of_code
import           Helpers                        ( readCommaSeparatedInts
                                                )
import           Computer.IntCode               ( IntCode
                                                , at
                                                , withNounAndVerb
                                                )


data OpCode = Addition
            | Multiplication
            deriving Show

data ParameterMode = Position
                   | Immediate
                   deriving Show

type Instruction = (OpCode, [ParameterMode])


run :: IntCode -> Int
run = (`at` 0) . run' 0
 where
  run' index intCode | intCode `at` index == 99 = intCode
                     | otherwise                = run' nextIndex updatedIntCode
   where
    nextIndex      = index + 4
    updatedIntCode = execOpcodeAt index intCode


runNounVerb :: Int -> Int -> IntCode -> Int
runNounVerb noun verb = run . withNounAndVerb noun verb


execOpcodeAt :: Int -> IntCode -> IntCode
execOpcodeAt index intCode = Seq.update targetIndex newVal intCode
 where
  (opcode, _) = readInstruction (intCode `at` index)
  a           = intCode `at` (intCode `at` (index + 1))
  b           = intCode `at` (intCode `at` (index + 2))
  targetIndex = intCode `at` (index + 3)
  newVal      = case opcode of
    Addition       -> a + b
    Multiplication -> a * b


readInstruction :: Int -> Instruction
readInstruction = read' . reverse . show
 where
  read' :: String -> Instruction
  read' [x           ] = (charsToOpcode ('0', x), [])
  read' (x : y : rest) = (charsToOpcode (y, x), map charToMode rest)

  charsToOpcode :: (Char, Char) -> OpCode
  charsToOpcode (_, '1') = Addition
  charsToOpcode (_, '2') = Multiplication

  charToMode :: Char -> ParameterMode
  charToMode '0' = Position
  charToMode '1' = Immediate
