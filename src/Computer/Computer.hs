module Computer.Computer
  ( run
  , runNounVerb
  )
where

import           Data.Foldable                  ( toList )
import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq
                                                , Seq((:<|))
                                                )

import           Paths_advent_of_code
import           Helpers                        ( readCommaSeparatedInts )
import           Computer.IntCode               ( IntCode
                                                , at
                                                , takeNAt
                                                , updateAt
                                                , withNounAndVerb
                                                )


data OpCode = Addition
            | Multiplication
            | Input
            | Output
            | Halt
            deriving (Show, Eq)

data ParameterMode = Position
                   | Immediate
                   deriving Show

type Operation = [(ParameterMode, Int)] -> IntCode -> IntCode

run :: IntCode -> IntCode
run = execOpcodeAt 0


runNounVerb :: Int -> Int -> IntCode -> IntCode
runNounVerb noun verb = run . withNounAndVerb noun verb


execOpcodeAt :: Int -> IntCode -> IntCode
execOpcodeAt index intCode | opcode == Halt = intCode
                           | otherwise = execOpcodeAt nextIndex updatedIntCode
 where
  (rawInstruction :<| args) = takeNAt 4 index intCode
  (opcode, paramModes)      = parseInstruction rawInstruction
  paramsWithModes           = zipPadded paramModes (toList args)

  (operation, nextIndex)    = case opcode of
    Addition       -> (execBasicUpdate (+), index + 4)
    Multiplication -> (execBasicUpdate (*), index + 4)
    Input          -> (execInput, index + 2)
    Output         -> (\_ _ -> intCode, index + 2)

  updatedIntCode = operation paramsWithModes intCode


zipPadded :: [ParameterMode] -> [Int] -> [(ParameterMode, Int)]
zipPadded []             nums         = zip (repeat Position) nums
zipPadded (mode : modes) (num : nums) = (mode, num) : zipPadded modes nums


parseInstruction :: Int -> (OpCode, [ParameterMode])
parseInstruction = read' . reverse . show
 where
  read' :: String -> (OpCode, [ParameterMode])
  read' [x           ] = (charsToOpcode ('0', x), [])
  read' (x : y : rest) = (charsToOpcode (y, x), map charToMode rest)

  charsToOpcode :: (Char, Char) -> OpCode
  charsToOpcode (_  , '1') = Addition
  charsToOpcode (_  , '2') = Multiplication
  charsToOpcode (_  , '3') = Input
  charsToOpcode (_  , '4') = Output
  charsToOpcode ('9', '9') = Halt

  charToMode :: Char -> ParameterMode
  charToMode '0' = Position
  charToMode '1' = Immediate


execBasicUpdate
  :: (Int -> Int -> Int) -> Operation
execBasicUpdate basicOp [(modeA, a), (modeB, b), (_, targetIndex)] intCode =
  updateAt targetIndex (basicOp a' b') intCode
 where
  a' = case modeA of
    Immediate -> a
    Position  -> intCode `at` a
  b' = case modeB of
    Immediate -> b
    Position  -> intCode `at` b


execInput :: Operation
execInput ((_, targetIndex):_) = updateAt targetIndex 1
