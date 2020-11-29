#!/usr/bin/env stack
{- stack
  script
  --resolver lts-16.23
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
import           Data.Vector.Unboxed            ( Vector )
import           Data.List.Split                ( splitOn )
import           IntCode

input :: IO String
input = head . lines <$> readFile "../day02input.txt"

readInputs :: String -> Vector Int
readInputs l = V.fromList $ readInt <$> splitOn "," l
 where
  readInt :: String -> Int
  readInt = read

compute :: Vector Int -> Vector Int
compute state = sMemory $ run $ initial state

solve :: String -> Int
solve s = V.head $ compute state
 where
  stateOrig = readInputs s
  state     = V.update stateOrig (V.fromList [(1, 12), (2, 2)])

tests = describe "solve" $ specFromExamples
  [ ([1, 0, 0, 0, 99]             , [2, 0, 0, 0, 99])
  , ([2, 3, 0, 3, 99]             , [2, 3, 0, 6, 99])
  , ([2, 4, 4, 5, 99, 0]          , [2, 4, 4, 5, 99, 9801])
  , ([1, 1, 1, 4, 99, 5, 6, 0, 99], [30, 1, 1, 4, 2, 5, 6, 0, 99])
  ]
  (\(input, expected) ->
    specItem (show input ++ " should be computed into: " ++ show expected)
      $          compute (V.fromList input)
      `shouldBe` V.fromList expected
  )

main :: IO ()
main = do
  hspec tests
  inp <- input
  print $ solve inp
