{-# LANGUAGE NamedFieldPuns #-}

module IntCode
  ( run
  , Instruction(..)
  , Parameter(..)
  , MachineMode(..)
  , State(..)
  , initial
  , withInput
  )
where

import qualified Data.Vector.Unboxed           as V
import           Data.Vector.Unboxed            ( Vector
                                                , (!)
                                                , (!?)
                                                )

run :: State -> State
run state = case sMode state of
  Running -> run (step state)
  Halted  -> state

data Instruction =
    Mul (Parameter Int, Parameter Int, Int)
  | Add (Parameter Int, Parameter Int, Int)
  | Halt
  | Input Int
  | Output (Parameter Int)

data Parameter a = PositionMode a | ImmediateMode a

data MachineMode = Running | Halted deriving Show

data State = State {
  sIP :: Int,
  sMemory :: Vector Int,
  sInput :: [Int],
  sOutput :: [Int],
  sMode :: MachineMode
  } deriving (Show)

singleton :: a -> [a]
singleton x = [x]

instruction :: State -> Instruction
instruction State { sMemory, sIP } =
  case (opcode, V.toList $ V.slice (sIP + 1) 3 sMemory) of
    (1 , [x, y, dest]) -> Add (modeForParam 0 x, modeForParam 1 y, dest)
    (2 , [x, y, dest]) -> Mul (modeForParam 0 x, modeForParam 1 y, dest)
    (3 , x : _       ) -> Input x
    (4 , x : _       ) -> Output $ modeForParam 0 x
    (99, _           ) -> Halt
    (x, _) -> error $ "unknown instruction: " ++ show x ++ " @ " ++ show sIP
 where
  int                         = sMemory ! sIP
  (opcodeCharsRev, modeChars) = splitAt 2 $ reverse $ show int
  opcode                      = read $ reverse opcodeCharsRev
  modes :: Vector Int
  modes = V.fromList $ read . singleton <$> modeChars
  modeFromInt 0 = PositionMode
  modeFromInt 1 = ImmediateMode
  modeFromInt x = error $ "invalid mode for parameter: " ++ show x
  modeForParam :: Int -> Int -> Parameter Int
  modeForParam index = maybe PositionMode modeFromInt (modes !? index)

step :: State -> State
step state = case instruction state of
  Mul (xi, yi, di) -> calc2dest xi yi di (*)
  Add (xi, yi, di) -> calc2dest xi yi di (+)
  Halt             -> state { sMode = Halted, sIP = incIp 1 }
  Input p ->
    let (i : is) = sInput state
        mem'     = modify p i
    in  state { sInput = is, sMemory = mem', sIP = incIp 2 }
  Output i -> state { sOutput = sOutput state ++ [get i], sIP = incIp 2 }
 where
  incIp x = sIP state + x
  memory = sMemory state
  get :: Parameter Int -> Int
  get (PositionMode  i) = memory ! i
  get (ImmediateMode i) = i
  modify :: Int -> Int -> Vector Int
  modify dest value = V.update memory (V.fromList [(dest, value)])
  calc2dest
    :: Parameter Int -> Parameter Int -> Int -> (Int -> Int -> Int) -> State
  calc2dest xi yi di op =
    let x = get xi
        y = get yi
        r = op x y
    in  state { sIP = incIp 4, sMemory = modify di r }

initial :: Vector Int -> State
initial memory = State { sIP     = 0
                       , sMode   = Running
                       , sMemory = memory
                       , sOutput = []
                       , sInput  = []
                       }

withInput :: [Int] -> State -> State
withInput input s = s { sInput = input }

