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


data Opcode = Addition
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

type ComputerState = State (IntCode, Output)
type Operation = [(ParameterMode, Int)] -> ComputerState ()
type IndexOperation = [(ParameterMode, Int)] -> Index -> ComputerState Index
type Output = [Int]
type Input = [Int]
type Index = Int


toParamMode :: Int -> ParameterMode
toParamMode 0 = Position
toParamMode 1 = Immediate


toOpcode :: Int -> Opcode
toOpcode 1  = Addition
toOpcode 2  = Multiplication
toOpcode 3  = Input
toOpcode 4  = Output
toOpcode 5  = JumpIfTrue
toOpcode 6  = JumpIfFalse
toOpcode 7  = LessThan
toOpcode 8  = Equals
toOpcode 99 = Halt


run :: Input -> IntCode -> Output
run inputList intCode = evalState (execOpcodes 0 inputList) (intCode, [])


runForIntCode :: IntCode -> IntCode
runForIntCode intCode = resultIntCode
    where (resultIntCode, _) = execState (execOpcodes 0 []) (intCode, [])


execOpcodes :: Index -> Input -> ComputerState Output
execOpcodes index inputList = do
    (opcode, nextIndex) <- execOpcodeAt index inputList

    case opcode of
        Halt  -> snd <$> get
        Input -> execOpcodes nextIndex (tail inputList)
        _     -> execOpcodes nextIndex inputList


execOpcodeAt :: Index -> Input -> ComputerState (Opcode, Index)
execOpcodeAt index inputList = do
    (opcode, paramsWithModes) <- parseSliceAt index
    nextIndex                 <- nextIndexFor opcode paramsWithModes index

    let execOpCode = case opcode of
            Addition       -> updateOpWith (+)
            Multiplication -> updateOpWith (*)
            LessThan       -> updateOpWith (boolToInt (<))
            Equals         -> updateOpWith (boolToInt (==))
            Input          -> inputOp (head inputList)
            Output         -> writeOutput
            _              -> const $ pure ()

    execOpCode paramsWithModes

    pure (opcode, nextIndex)

parseSliceAt :: Index -> ComputerState (Opcode, [(ParameterMode, Int)])
parseSliceAt index = do
    (intCode, _) <- get
    let (rawOpcode :<| args) = takeNAt 4 index intCode
    let (opcode, paramModes) = parseOpcode rawOpcode
    let paramsWithModes      = zip paramModes (toList args)

    pure (opcode, paramsWithModes)


parseOpcode :: Int -> (Opcode, [ParameterMode])
parseOpcode num = (opCode, paramModes)
  where
    opCode     = toOpcode (num `rem` 100)
    paramModes = map
        toParamMode
        [ num `div` 100 `rem` 10
        , num `div` 1000 `rem` 10
        , num `div` 10000 `rem` 10
        ]


updateOpWith :: (Int -> Int -> Int) -> Operation
updateOpWith basicOp [a, b, (_, targetIndex)] = do
    (intCode, outputList) <- get
    let a'         = readParam intCode a
    let b'         = readParam intCode b
    let newIntCode = updateAt targetIndex (basicOp a' b') intCode

    put (newIntCode, outputList)


boolToInt :: (Int -> Int -> Bool) -> Int -> Int -> Int
boolToInt comparator a b = if comparator a b then 1 else 0


inputOp :: Int -> Operation
inputOp inputVal ((_, targetIndex) : _) = do
    (intCode, outputList) <- get
    put (updateAt targetIndex inputVal intCode, outputList)


writeOutput :: Operation
writeOutput (param : _) = do
    (intCode, outputList) <- get
    put (intCode, readParam intCode param : outputList)


nextIndexFor :: Opcode -> IndexOperation
nextIndexFor Addition       = increaseBy 4
nextIndexFor Multiplication = increaseBy 4
nextIndexFor Input          = increaseBy 2
nextIndexFor Output         = increaseBy 2
nextIndexFor LessThan       = increaseBy 4
nextIndexFor Equals         = increaseBy 4
nextIndexFor JumpIfTrue     = jumpIf (/= 0)
nextIndexFor JumpIfFalse    = jumpIf (== 0)
nextIndexFor _              = \_ index -> pure index


jumpIf :: (Int -> Bool) -> IndexOperation
jumpIf condition (param : param2 : _) index = do
    (intCode, _) <- get

    if condition (readParam intCode param)
        then pure $ readParam intCode param2
        else pure $ index + 3


increaseBy :: Int -> IndexOperation
increaseBy num _ index = pure $ index + num


readParam :: IntCode -> (ParameterMode, Int) -> Int
readParam intCode (mode, x) = case mode of
    Position  -> intCode `at` x
    Immediate -> x
