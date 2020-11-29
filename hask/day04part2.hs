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
import Data.List (group)

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

    groups = group digits
    includesSimilarAdjecent = (not . null) $ filter ((== 2). length) groups


solve :: [Int] -> Int
solve xs = length $ filter validPassword xs

tests = describe "validPassword" $ specFromExamples
  [(111111, False), (223450, False), (123789, False), (112233, True), (123444, False), (111122, True)]
  (\(input, expected) ->
    specItem ("validPassword(" ++ show input ++ ") should be: " ++ show expected)
      $          validPassword input
      `shouldBe` expected
  )

main :: IO ()
main = do
  hspec tests
  print $ solve input
