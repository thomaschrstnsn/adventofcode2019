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

import Data.List (elemIndex, nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
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

getPathToCom :: OrbitLookup -> Object -> [Object]
getPathToCom = getPathToCom' []
  where
    getPathToCom' :: [Object] -> OrbitLookup -> Object -> [Object]
    getPathToCom' p lookup obj =
      case Map.lookup obj lookup of
        Just o' -> getPathToCom' (o' : p) lookup o'
        Nothing -> reverse p

firstShared :: (Foldable t, Eq a) => t a -> [a] -> a
firstShared xs ys = head $ filter (`elem` xs) ys

solve :: [Orbit] -> Int
solve orbits = sum $ mapMaybe (elemIndex (firstShared p2San p2You)) [p2You, p2San]
  where
    lookup = buildLookup orbits
    p2You = getPathToCom lookup "YOU"
    p2San = getPathToCom lookup "SAN"

tests :: SpecWith ()
tests = do
  describe "solve" $
    it "works with example" $
      solve example
        `shouldBe` 4
  describe "getPathToCom" $ do
    it "works with YOU" $
      path2You `shouldBe` reverse ["COM", "B", "C", "D", "E", "J", "K"]
    it "works with SAN" $
      path2San `shouldBe` reverse ["COM", "B", "C", "D", "I"]
  describe "firstShared" $ do
    it "with SAN and YOU paths" $
      firstShared path2San path2You `shouldBe` "D"
    it "with YOU and SAN paths" $
      firstShared path2You path2San `shouldBe` "D"
  where
    exInput =
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
        "K)L",
        "K)YOU",
        "I)SAN"
      ]
    example = readInputs exInput
    lookup = buildLookup example
    path2You = getPathToCom lookup "YOU"
    path2San = getPathToCom lookup "SAN"

main :: IO ()
main = do
  inp <- input
  hspec tests
  print $ solve $ readInputs inp
