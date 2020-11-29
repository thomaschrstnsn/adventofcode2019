#!/usr/bin/env stack
{- stack
  script
  --resolver lts-16.23
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
import           Data.Function                  ( flip
                                                , (&)
                                                )

input :: IO [String]
input = lines <$> readFile "../day01input.txt"

readInputs :: [String] -> [Int]
readInputs = map readInt
 where
  readInt :: String -> Int
  readInt = read


calcFuel :: Int -> Int
calcFuel modWeight = fuelWeights
 where
  fuelWeights = calcFuel' modWeight 0
  formula :: Int -> Int
  formula x = x `div` 3 - 2
  calcFuel' :: Int -> Int -> Int
  calcFuel' f acc = if continue then calcFuel' r (acc + r) else acc
   where
    r        = formula f
    continue = r > 0


solve :: [String] -> Int
solve xs = (calcFuel <$> readInputs xs) & sum

tests = describe "calcFuel" $ specFromExamples
  [(14, 2), (1969, 966), (100756, 50346)]
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
