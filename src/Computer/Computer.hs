module Computer.Computer
    ( run
    , runWithInput
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
            | JumpIfTrue
            | JumpIfFalse
            | LessThan
            | Equals
            | Halt
            deriving (Show, Eq)

data ParameterMode = Position | Immediate deriving Show

type Operation = [(ParameterMode, Int)] -> IntCode -> IntCode
type IndexOperation = [(ParameterMode, Int)] -> IntCode -> Int -> Int
type Output = [Int]
type Input = Maybe Int


toParamMode :: Int -> ParameterMode
toParamMode 0 = Position
toParamMode 1 = Immediate


toOpCode :: Int -> OpCode
toOpCode 1  = Addition
toOpCode 2  = Multiplication
toOpCode 3  = Input
toOpCode 4  = Output
toOpCode 5  = JumpIfTrue
toOpCode 6  = JumpIfFalse
toOpCode 7  = LessThan
toOpCode 8  = Equals
toOpCode 99 = Halt


run :: IntCode -> (IntCode, Output)
run = execOpcodes Nothing 0 []


runNounVerb :: Int -> Int -> IntCode -> (IntCode, Output)
runNounVerb noun verb = run . withNounAndVerb noun verb


runWithInput :: Int -> IntCode -> (IntCode, Output)
runWithInput inputVal = execOpcodes (Just inputVal) 0 []


execOpcodes :: Input -> Int -> Output -> IntCode -> (IntCode, Output)
execOpcodes inputVal = execOpcodes'
  where
    operationFor' = operationFor inputVal

    execOpcodes' :: Int -> Output -> IntCode -> (IntCode, Output)
    execOpcodes' index outputList intCode
        | opcode == Halt = (intCode, outputList)
        | otherwise = execOpcodes' nextIndex updatedOutputList updatedIntCode
      where
        (rawOpcode :<| args) = takeNAt 4 index intCode
        (opcode, paramModes) = parseOpcode rawOpcode
        paramsWithModes      = zip paramModes (toList args)
        operation            = operationFor' opcode
        nextIndex            = nextIndexFor opcode paramsWithModes intCode index

        updatedOutputList    = case opcode of
            Output -> updateOuput paramsWithModes intCode outputList
            _      -> outputList

        updatedIntCode = operation paramsWithModes intCode


parseOpcode :: Int -> (OpCode, [ParameterMode])
parseOpcode num = (opCode, paramModes)
  where
    opCode     = toOpCode (num `rem` 100)
    paramModes = map
        toParamMode
        [ num `div` 100 `rem` 10
        , num `div` 1000 `rem` 10
        , num `div` 10000 `rem` 10
        ]


updateWith :: (Int -> Int -> Int) -> Operation
updateWith basicOp [a, b, (_, targetIndex)] intCode = updateAt
    targetIndex
    (basicOp a' b')
    intCode
  where
    a' = readParam intCode a
    b' = readParam intCode b


inputOp :: Input -> Operation
inputOp Nothing         _                      = id
inputOp (Just inputVal) ((_, targetIndex) : _) = updateAt targetIndex inputVal


updateOuput :: [(ParameterMode, Int)] -> IntCode -> Output -> Output
updateOuput (param : _) intCode output = readParam intCode param : output


operationFor :: Input -> OpCode -> Operation
operationFor _        Addition       = updateWith (+)
operationFor _        Multiplication = updateWith (*)
operationFor inputVal Input          = inputOp inputVal
operationFor _        Output         = noOp
operationFor _        JumpIfTrue     = noOp
operationFor _        JumpIfFalse    = noOp
operationFor _        LessThan       = updateWith (boolOp (<))
operationFor _        Equals         = updateWith (boolOp (==))


nextIndexFor :: OpCode -> IndexOperation
nextIndexFor Addition       = increaseBy 4
nextIndexFor Multiplication = increaseBy 4
nextIndexFor Input          = increaseBy 2
nextIndexFor Output         = increaseBy 2
nextIndexFor LessThan       = increaseBy 4
nextIndexFor Equals         = increaseBy 4
nextIndexFor JumpIfTrue     = jumpIf (/= 0)
nextIndexFor JumpIfFalse    = jumpIf (== 0)


noOp :: Operation
noOp _ intCode = intCode


boolOp :: (Int -> Int -> Bool) -> Int -> Int -> Int
boolOp comparator a b = if comparator a b then 1 else 0


jumpIf :: (Int -> Bool) -> IndexOperation
jumpIf condition (param : param2 : _) intCode index =
    if condition (readParam intCode param)
        then readParam intCode param2
        else index + 3


increaseBy :: Int -> IndexOperation
increaseBy num _ _ index = index + num


readParam :: IntCode -> (ParameterMode, Int) -> Int
readParam intCode (mode, x) = case mode of
    Position  -> intCode `at` x
    Immediate -> x
