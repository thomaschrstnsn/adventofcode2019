#!/usr/bin/env stack
{- stack
  script
  --resolver lts-12.26
  --package hspec
  --package hspec-core
  --package split
  --package containers
-}
{-# LANGUAGE NamedFieldPuns #-}
import           Specs                          ( specFromExamples
                                                , specItem
                                                )
import           Test.Hspec                     ( describe
                                                , hspec
                                                , shouldBe
                                                )
import           Data.List.Split                ( splitOn )
import           Data.List                      ( sort )
import           Data.Maybe                     ( catMaybes )
import           Data.Set                       ( Set , fromList)

input :: IO (String, String)
input = do
  f <- readFile "../day03input.txt"
  let x : y : _ = lines f
  return (x, y)

readInput :: String -> [PathSegment]
readInput l = readOne <$> splitOn "," l
 where
  readOne :: String -> PathSegment
  readOne (x : xs) = case x of
    'U' -> (DUp, dist)
    'D' -> (DDown, dist)
    'L' -> (DLeft, dist)
    'R' -> (DRight, dist)
    _   -> error $ "could not parse: " ++ show [x : xs]
    where dist = read xs
  readOne "" = error "could not parse empty string"

data Direction = DUp | DDown | DLeft | DRight deriving (Show, Eq)

type Distance = Int
type PathSegment = (Direction, Distance)
type Point = (Int, Int)

data LineSegment = Horizontal { hY :: Int, hXs :: (Int, Int)} |
    Vertical { vX :: Int, vYs :: (Int, Int)} deriving (Show, Eq)

linesFromPaths :: [PathSegment] -> [LineSegment]
linesFromPaths ps = lfp (0, 0) ps []
 where
  lfp :: (Int, Int) -> [PathSegment] -> [LineSegment] -> [LineSegment]
  lfp _      []                 res = reverse res
  lfp (x, y) ((dir, dist) : ps) res = case dir of
    DUp    -> lfp (x, y + dist) ps $ Vertical x (y, y + dist) : res
    DDown  -> lfp (x, y - dist) ps $ Vertical x (y, y - dist) : res
    DLeft  -> lfp (x - dist, y) ps $ Horizontal y (x, x - dist) : res
    DRight -> lfp (x + dist, y) ps $ Horizontal y (x, x + dist) : res

intersect' :: Point -> Point -> Point -> Maybe Point
intersect' (px, py) rx ry = if intersects then Just (px, py) else Nothing
 where
  range (x1, x2) =
    let xmin = min x1 x2
        xmax = max x1 x2
    in  [xmin .. xmax]
  xsOverlap  = px `elem` range rx
  ysOverlap  = py `elem` range ry
  intersects = xsOverlap && ysOverlap

intersect :: (LineSegment, LineSegment) -> Maybe Point
intersect (Horizontal { hY, hXs }, Vertical { vX, vYs }) =
  intersect' (vX, hY) hXs vYs
intersect (Vertical { vX, vYs }, Horizontal { hY, hXs }) =
  intersect' (vX, hY) hXs vYs
intersect _ = Nothing

intersections :: ([LineSegment], [LineSegment]) -> [Point]
intersections (ls1, ls2) =
  filter (/= (0, 0)) $ catMaybes $ intersect <$> combinations
  where combinations = [ (x, y) | x <- ls1, y <- ls2 ]

distanceFromOrigo :: Point -> Int
distanceFromOrigo (x, y) = abs x + abs y

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

solve :: (String, String) -> Int
solve sp = minimum (distanceFromOrigo <$> intersections lsp)
 where
  psp = mapBoth readInput sp
  lsp = mapBoth linesFromPaths psp

tests = do
  describe "readInput" $ specFromExamples
    [ ("R8,U5,L5,D3", [(DRight, 8), (DUp, 5), (DLeft, 5), (DDown, 3)])
    , ("U7,R6,D4,L4", [(DUp, 7), (DRight, 6), (DDown, 4), (DLeft, 4)])
    ]
    (\(input, expected) ->
      specItem (show input ++ " should be parsed as: " ++ show expected)
        $          readInput input
        `shouldBe` expected
    )
  describe "intersections" $ specFromExamples
    [("R8,U5,L5,D3", "U7,R6,D4,L4", [(3, 3), (6, 5)]),
    ( "R75,D30,R83,U83,L12,D49,R71,U7,L72"
      , "U62,R66,U55,R34,D71,R55,D58,R83"
      , [(146,46),(155,4),(155,11),(158,-12)]
      )]
    (\(first, second, expected) ->
      specItem
          (  show first
          ++ " with "
          ++ second
          ++ " should yield: "
          ++ show expected
          )
        $ let lsp    = mapBoth (linesFromPaths . readInput) (first, second)
              actual = intersections lsp
          in  fromList actual `shouldBe` fromList expected
    )
  describe "solve" $ specFromExamples
    [ ("R8,U5,L5,D3", "U7,R6,D4,L4", 6)
    , ( "R75,D30,R83,U83,L12,D49,R71,U7,L72"
      , "U62,R66,U55,R34,D71,R55,D58,R83"
      , 159
      )
    , ( "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
      , "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
      , 135
      )
    ]
    (\(first, second, expected) ->
      specItem
          (  show first
          ++ " with "
          ++ second
          ++ " should yield: "
          ++ show expected
          )
        $          solve (first, second)
        `shouldBe` expected
    )


main :: IO ()
main = do
  hspec tests
  inp <- input
  print $ solve inp
