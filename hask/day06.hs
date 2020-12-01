#!/usr/bin/env stack
{- stack
  script
  --resolver lts-16.23
  --package hspec
  --package hspec-core
  --package text
  --package containers
-}

{-# LANGUAGE OverloadedStrings #-}

import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Hspec
  ( SpecWith,
    describe,
    hspec,
    it,
    shouldBe,
  )

type Object = T.Text

type Orbit = (Object, Object)

input :: IO [T.Text]
input = do
  f <- readFile "../day06input.txt"
  let l = lines f
  return $ map T.pack l

readInputs :: [T.Text] -> [Orbit]
readInputs ls = readInput <$> ls

readInput :: T.Text -> Orbit
readInput l = (head split, split !! 1)
  where
    split = T.splitOn ")" l

distinctObjects :: [Orbit] -> [Object]
distinctObjects orbits = nub $ map fst orbits ++ map snd orbits

type OrbitLookup = Map.Map Object Object

buildLookup :: [Orbit] -> OrbitLookup
buildLookup = foldl (\m o -> Map.insert (snd o) (fst o) m) Map.empty

countOrbits :: OrbitLookup -> Object -> Int
countOrbits = countOrbits' 0
  where
    countOrbits' :: Int -> OrbitLookup -> Object -> Int
    countOrbits' n lookup obj =
      case Map.lookup obj lookup of
        Just o' -> countOrbits' (n + 1) lookup o'
        Nothing -> n

solve :: [Orbit] -> Int
solve orbits = sum $ map (countOrbits lookup) objects
  where
    lookup = buildLookup orbits
    objects = distinctObjects orbits

tests :: SpecWith ()
tests =
  describe "solve" $
    it "works with example" $
      let input =
            [ "COM)B",
              "B)C",
              "C)D",
              "D)E",
              "E)F",
              "B)G",
              "G)H",
              "D)I",
              "E)J",
              "J)K",
              "K)L"
            ]
          parsed = readInputs input
       in solve parsed `shouldBe` 42

main :: IO ()
main = do
  inp <- input
  hspec tests
  print $ solve $ readInputs inp
