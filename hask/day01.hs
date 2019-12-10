#!/usr/bin/env stack
{- stack
  script
  --resolver lts-12.26
  --package hspec
  --package hspec-core
-}
import           Specs                          ( specFromExamples
                                                , specItem
                                                )
import           Test.Hspec                     ( describe
                                                , hspec
                                                , shouldBe
                                                )

input :: IO [String]
input = lines <$> readFile "../day01input.txt"

readInputs :: [String] -> [Int]
readInputs = map readInt
 where
  readInt :: String -> Int
  readInt = read

calcFuel :: Int -> Int
calcFuel x = x `div` 3 - 2

solve :: [String] -> Int
solve xs = sum $ calcFuel <$> readInputs xs

tests = describe "calcFuel" $ specFromExamples
  [(12, 2), (14, 2), (1969, 654), (100756, 33583)]
  (\(input, expected) ->
    specItem (show input ++ " should be: " ++ show expected)
      $          calcFuel input
      `shouldBe` expected
  )

main :: IO ()
main = do
  hspec tests
  inp <- input
  print $ solve inp
