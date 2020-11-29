#!/usr/bin/env stack
{- stack
  script
  --resolver lts-16.23
  --package hspec
  --package hspec-core
  --package vector
  --package split
-}
{-# LANGUAGE NamedFieldPuns #-}
import           Specs                          ( specFromExamples
                                                , specItem
                                                )
import           Test.Hspec                     ( describe
                                                , hspec
                                                , shouldBe
                                                )
import           IntCode
import qualified Data.Vector.Unboxed           as V
import           Data.Vector.Unboxed            ( Vector )
import           Data.List.Split                ( splitOn )


input :: IO String
input = head . lines <$> readFile "../day05input.txt"

readInputs :: String -> Vector Int
readInputs l = V.fromList $ readInt <$> splitOn "," l
 where
  readInt :: String -> Int
  readInt = read

solve :: String -> State
solve s = finalState
 where
  memory       = readInputs s
  initialState = withInput [1] $ initial memory
  finalState   = run initialState

tests = describe "solve" $ specFromExamples
  [ ([1, 0, 0, 0, 99]             , [2, 0, 0, 0, 99])
  , ([2, 3, 0, 3, 99]             , [2, 3, 0, 6, 99])
  , ([2, 4, 4, 5, 99, 0]          , [2, 4, 4, 5, 99, 9801])
  , ([1, 1, 1, 4, 99, 5, 6, 0, 99], [30, 1, 1, 4, 2, 5, 6, 0, 99])
  , ([1101, 42, 1, 0, 99]         , [43, 42, 1, 0, 99])
  ]
  (\(input, expected) ->
    specItem (show input ++ " should be computed into: " ++ show expected)
      $          (sMemory . run) (initial $ V.fromList input)
      `shouldBe` V.fromList expected
  )

main :: IO ()
main = do
  hspec tests
  inp <- input
  print $ solve inp
