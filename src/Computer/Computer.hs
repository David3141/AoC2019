module Computer.Computer
    ( run
    , runForIntCode
    )
where

import           Data.Foldable                  ( toList )
import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq
                                                , Seq((:<|))
                                                )
import           Control.Monad.Trans.State

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
type IndexOperation = [(ParameterMode, Int)] -> IntCode -> Index -> Index
type Output = [Int]
type Input = [Int]
type Index = Int


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


run :: Input -> IntCode -> Output
run inputList intCode = evalState (execOpcodes 0 inputList) (intCode, [])


runForIntCode :: IntCode -> IntCode
runForIntCode intCode = resultIntCode
    where (resultIntCode, _) = execState (execOpcodes 0 []) (intCode, [])


execOpcodes :: Index -> Input -> State (IntCode, Output) Output
execOpcodes index inputList = do
    (intCode, outputList) <- get

    let (opcode, paramsWithModes, nextIndex) = parseSliceAt index intCode

    case opcode of

        Addition -> do
            put (updateWith (+) paramsWithModes intCode, outputList)
            execOpcodes nextIndex inputList

        Multiplication -> do
            put (updateWith (*) paramsWithModes intCode, outputList)
            execOpcodes nextIndex inputList

        Input -> do
            let (input : inputRest) = inputList

            put (inputOp input paramsWithModes intCode, outputList)
            execOpcodes nextIndex inputRest

        Output -> do
            put (intCode, updateOuput paramsWithModes intCode outputList)
            execOpcodes nextIndex inputList

        JumpIfTrue  -> execOpcodes nextIndex inputList

        JumpIfFalse -> execOpcodes nextIndex inputList

        LessThan    -> do
            put (updateWith (boolOp (<)) paramsWithModes intCode, outputList)
            execOpcodes nextIndex inputList

        Equals -> do
            put (updateWith (boolOp (==)) paramsWithModes intCode, outputList)
            execOpcodes nextIndex inputList

        Halt -> return outputList

    -- let operation                            = operationFor input opcode
    -- let newIntCode                           = operation paramsWithModes intCode

    -- let newOutputList = case opcode of
    --         Output -> updateOuput paramsWithModes intCode outputList
    --         _      -> outputList
    -- let newInputList = case opcode of
    --         Input -> inputRest
    --         _     -> input : inputRest

    -- put (newIntCode, newOutputList)

    -- if opcode == Halt
    --     then return newOutputList
    --     else execOpcodes nextIndex newInputList


parseSliceAt :: Index -> IntCode -> (OpCode, [(ParameterMode, Int)], Int)
parseSliceAt index intCode = (opcode, paramsWithModes, nextIndex)
  where
    (rawOpcode :<| args) = takeNAt 4 index intCode
    (opcode, paramModes) = parseOpcode rawOpcode
    paramsWithModes      = zip paramModes (toList args)
    nextIndex            = nextIndexFor opcode paramsWithModes intCode index


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


inputOp :: Int -> Operation
inputOp inputVal ((_, targetIndex) : _) = updateAt targetIndex inputVal


updateOuput :: [(ParameterMode, Int)] -> IntCode -> Output -> Output
updateOuput (param : _) intCode output = readParam intCode param : output


operationFor :: Int -> OpCode -> Operation
operationFor _        Addition       = updateWith (+)
operationFor _        Multiplication = updateWith (*)
operationFor inputVal Input          = inputOp inputVal
operationFor _        Output         = noOp
operationFor _        JumpIfTrue     = noOp
operationFor _        JumpIfFalse    = noOp
operationFor _        LessThan       = updateWith (boolOp (<))
operationFor _        Equals         = updateWith (boolOp (==))
operationFor _        _              = \_ intCode -> intCode


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
