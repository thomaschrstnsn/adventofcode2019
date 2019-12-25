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
  | JumpIfTrue (Parameter Int, Parameter Int)
  | JumpIfFalse (Parameter Int, Parameter Int)
  | LessThan (Parameter Int, Parameter Int, Int)
  | Equals (Parameter Int, Parameter Int, Int)
  | Halt
  | Input Int
  | Output (Parameter Int)
  deriving Show

data Parameter a = PositionMode a | ImmediateMode a deriving Show

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
instruction State { sMemory, sIP } = case opcode of
  1 ->
    let [x, y, dest] = getParams 3
    in  Add (modeForParam 0 x, modeForParam 1 y, dest)
  2 ->
    let [x, y, dest] = getParams 3
    in  Mul (modeForParam 0 x, modeForParam 1 y, dest)
  3 -> let [x] = getParams 1 in Input x
  4 -> let [x] = getParams 1 in Output $ modeForParam 0 x
  5 ->
    let [c, dst] = getParams 2
    in  JumpIfTrue (modeForParam 0 c, modeForParam 1 dst)
  6 ->
    let [c, dst] = getParams 2
    in  JumpIfFalse (modeForParam 0 c, modeForParam 1 dst)
  7 ->
    let [x, y, dst] = getParams 3
    in  LessThan (modeForParam 0 x, modeForParam 1 y, dst)
  8 ->
    let [x, y, dst] = getParams 3
    in  Equals (modeForParam 0 x, modeForParam 1 y, dst)
  99 -> Halt
  x  -> error $ "unknown instruction: " ++ show x ++ " @ " ++ show sIP
 where
  getParams n = V.toList $ V.slice (sIP + 1) n sMemory
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
  Input p          -> if null $ sInput state
    then error $ "no input for instruction: " ++ show (Input p)
    else
      let (i : is) = sInput state
          mem'     = modify p i
      in  state { sInput = is, sMemory = mem', sIP = incIp 2 }
  Output i -> state { sOutput = sOutput state ++ [get i], sIP = incIp 2 }
  JumpIfTrue  (c, dst)    -> jmpIf (/=) c dst
  JumpIfFalse (c, dst)    -> jmpIf (==) c dst
  LessThan    (x, y, dst) -> compare (<) x y dst
  Equals      (x, y, dst) -> compare (==) x y dst
 where
  incIp x = sIP state + x
  memory = sMemory state
  get :: Parameter Int -> Int
  get (PositionMode  i) = memory ! i
  get (ImmediateMode i) = i
  modify :: Int -> Int -> Vector Int
  modify dest value = V.update memory (V.fromList [(dest, value)])
  compare
    :: (Int -> Int -> Bool) -> Parameter Int -> Parameter Int -> Int -> State
  compare op x y dst =
    let val = if get x `op` get y then 1 else 0
    in  state { sIP = incIp 4, sMemory = modify dst val }
  jmpIf cmp c dst =
    let nextIp = if get c `cmp` 0 then get dst else incIp 3
    in  state { sIP = nextIp }
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

