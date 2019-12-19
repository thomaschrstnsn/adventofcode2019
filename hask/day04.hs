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

input :: [Int]
input = [356261 .. 846303]

validPassword :: Int -> Bool
validPassword i = isSixDigits && isIncreasing && includesSimilarAdjecent
  where
    singleton :: a -> [a]
    singleton x = [x]

    digits :: [Int]
    digits = read . singleton <$> show i
    
    isSixDigits = length digits == 6
    
    isIncreasing' prev [] = True
    isIncreasing' prev (x:xs) = (prev <= x) && isIncreasing' x xs
    isIncreasing = isIncreasing' 0 digits

    includesSimilarAdjecent' prev [] = False
    includesSimilarAdjecent' prev (x:xs) = (prev == x) || includesSimilarAdjecent' x xs
    includesSimilarAdjecent = includesSimilarAdjecent' (-1) digits


solve :: [Int] -> Int
solve xs = length $ filter validPassword xs

tests = describe "validPassword" $ specFromExamples
  [(111111, True), (223450, False), (123789, False)]
  (\(input, expected) ->
    specItem ("validPassword(" ++ show input ++ ") should be: " ++ show expected)
      $          validPassword input
      `shouldBe` expected
  )

main :: IO ()
main = do
  hspec tests
  print $ solve input
