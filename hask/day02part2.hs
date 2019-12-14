#!/usr/bin/env stack
{- stack
  script
  --resolver lts-12.26
  --package hspec
  --package hspec-core
  --package vector
  --package split
-}
import           Specs                          ( specFromExamples
                                                , specItem
                                                )
import           Test.Hspec                     ( describe
                                                , hspec
                                                , shouldBe
                                                )
import qualified Data.Vector.Unboxed           as V
import           Data.Vector.Unboxed            ( Vector
                                                , (!)
                                                )
import           Data.List.Split                ( splitOn )

input :: IO String
input = head . lines <$> readFile "../day02input.txt"

readInputs :: String -> Vector Int
readInputs l = V.fromList $ readInt <$> splitOn "," l
 where
  readInt :: String -> Int
  readInt = read

compute :: Int -> Vector Int -> Vector Int
compute offset state = case step offset state of
  (state', Just offset') -> compute offset' state'
  (state', Nothing     ) -> state'

data Instruction =
  Mul (Int, Int, Int)
  | Add (Int, Int, Int)
  | Halt

step :: Int -> Vector Int -> (Vector Int, Maybe Int)
step offset state = case instruction of
  Mul (xi, yi, di) -> calc xi yi di (*)
  Add (xi, yi, di) -> calc xi yi di (+)
  Halt             -> (state, Nothing)
 where
  instruction :: Instruction
  instruction =
    case (state ! offset, V.toList $ V.slice (offset + 1) 3 state) of
      (1 , [x, y, dest]) -> Add (x, y, dest)
      (2 , [x, y, dest]) -> Mul (x, y, dest)
      (99, _           ) -> Halt
      (x, _) ->
        error $ "unknown instruction: " ++ show x ++ " @ " ++ show offset
  modify :: Int -> Int -> Vector Int
  modify dest value = V.update state (V.fromList [(dest, value)])
  calc :: Int -> Int -> Int -> (Int -> Int -> Int) -> (Vector Int, Maybe Int)
  calc xi yi di op =
    let x = state ! xi
        y = state ! yi
        r = op x y
    in  (modify di r, Just $ offset + 4)

desiredResult = 19690720

tryPair :: Vector Int -> (Int, Int) -> Int
tryPair origState (x, y) = V.head $ compute 0 state
  where state = V.update origState (V.fromList [(1, x), (2, y)])

solve :: String -> (Int, Int, Int, Int)
solve s = (100 * noun + verb, noun, verb, length matches)
 where
  stateOrig    = readInputs s
  pairs        = [ (x, y) | x <- [0 .. 99], y <- [0 .. 99] ]
  matches      = filter (\p -> tryPair stateOrig p == desiredResult) pairs
  (noun, verb) = head matches

tests = describe "solve" $ specFromExamples
  [ ([1, 0, 0, 0, 99]             , [2, 0, 0, 0, 99])
  , ([2, 3, 0, 3, 99]             , [2, 3, 0, 6, 99])
  , ([2, 4, 4, 5, 99, 0]          , [2, 4, 4, 5, 99, 9801])
  , ([1, 1, 1, 4, 99, 5, 6, 0, 99], [30, 1, 1, 4, 2, 5, 6, 0, 99])
  ]
  (\(input, expected) ->
    specItem (show input ++ " should be computed into: " ++ show expected)
      $          compute 0 (V.fromList input)
      `shouldBe` V.fromList expected
  )

main :: IO ()
main = do
  hspec tests
  inp <- input
  print $ solve inp
