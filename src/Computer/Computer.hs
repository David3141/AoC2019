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
            | Halt
            deriving (Show, Eq)

data ParameterMode = Position | Immediate deriving Show

type Operation = [(ParameterMode, Int)] -> IntCode -> IntCode
type Output = [Int]


toParamMode :: Int -> ParameterMode
toParamMode 0 = Position
toParamMode 1 = Immediate


toOpCode :: Int -> OpCode
toOpCode 1  = Addition
toOpCode 2  = Multiplication
toOpCode 3  = Input
toOpCode 4  = Output
toOpCode 99 = Halt


run :: IntCode -> (IntCode, Output)
run = execOpcodes Nothing 0 []


runWithInput :: Int -> IntCode -> (IntCode, Output)
runWithInput inputVal = execOpcodes (Just inputVal) 0 []


runNounVerb :: Int -> Int -> IntCode -> (IntCode, Output)
runNounVerb noun verb = run . withNounAndVerb noun verb


execOpcodes :: Maybe Int -> Int -> Output -> IntCode -> (IntCode, Output)
execOpcodes inputVal = execOpcodes'

  where
    execOpcodes' :: Int -> Output -> IntCode -> (IntCode, Output)
    execOpcodes' index outputList intCode
        | opcode == Halt = (intCode, outputList)
        | otherwise = execOpcodes' nextIndex updatedOutputList updatedIntCode

      where
        (rawInstruction :<| args) = takeNAt 4 index intCode
        (opcode, paramModes)      = parseInstruction rawInstruction
        paramsWithModes           = zip paramModes (toList args)
        (operation, nextIndex)    = case opcode of
            Addition       -> (execBasicUpdate (+), index + 4)
            Multiplication -> (execBasicUpdate (*), index + 4)
            Input          -> (execInput inputVal, index + 2)
            Output         -> (\_ intCode -> intCode, index + 2)

        updatedOutputList = case opcode of
            Output -> execOutput paramsWithModes intCode outputList
            _      -> outputList

        updatedIntCode = operation paramsWithModes intCode


parseInstruction :: Int -> (OpCode, [ParameterMode])
parseInstruction num = (opCode, paramModes)

  where
    opCode     = toOpCode (num `rem` 100)
    paramModes = map
        toParamMode
        [ num `div` 100 `rem` 10
        , num `div` 1000 `rem` 10
        , num `div` 10000 `rem` 10
        ]


execBasicUpdate :: (Int -> Int -> Int) -> Operation
execBasicUpdate basicOp [(modeA, a), (modeB, b), (_, targetIndex)] intCode =
    updateAt targetIndex (basicOp a' b') intCode

  where
    a' = case modeA of
        Immediate -> a
        Position  -> intCode `at` a
    b' = case modeB of
        Immediate -> b
        Position  -> intCode `at` b


execInput :: Maybe Int -> Operation
execInput Nothing _ = id
execInput (Just inputVal) ((_, targetIndex) : _) =
    updateAt targetIndex inputVal


execOutput :: [(ParameterMode, Int)] -> IntCode -> Output -> Output
execOutput ((mode, x) : _) intCode output = val : output

  where
    val = case mode of
        Position  -> intCode `at` x
        Immediate -> x
