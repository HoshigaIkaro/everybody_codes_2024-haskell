module Quests.Q14 (run, part1, part2, part3) where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

run :: IO ()
run = do
    let prefix = "./input/q14"
        firstInputName = prefix <> "/p1.txt"
        secondInputName = prefix <> "/p2.txt"
        thirdInputName = prefix <> "/p3.txt"
        trim = T.unpack . T.strip . T.pack
    firstInput <- trim <$> readFile firstInputName
    secondInput <- trim <$> readFile secondInputName
    thirdInput <- trim <$> readFile thirdInputName
    print $ part1 firstInput
    print $ part2 secondInput
    print $ part3 thirdInput

type Point = (Int, Int, Int)

data Direction = UP | DOWN | LEFT | RIGHT | FORWARD | BACKWARD deriving (Show, Eq, Ord)

type Distance = Int

data Step = Step Direction Distance deriving (Show)

charToDir :: Char -> Direction
charToDir 'U' = UP
charToDir 'D' = DOWN
charToDir 'L' = LEFT
charToDir 'R' = RIGHT
charToDir 'F' = FORWARD
charToDir _ = BACKWARD

pStep :: Parser Step
pStep = do
    dir <- charToDir <$> anySingle
    Step dir <$> L.decimal

pInput :: String -> [Step]
pInput = fromJust . parseMaybe (sepBy pStep (char ',')) . T.pack

applyStep :: Step -> Point -> Point
applyStep (Step dir dist) (r, c, d) = case dir of
    UP -> (r + dist, c, d)
    DOWN -> (r - dist, c, d)
    LEFT -> (r, c - dist, d)
    RIGHT -> (r, c + dist, d)
    FORWARD -> (r, c, d + dist)
    BACKWARD -> (r, c, d - dist)

simulate :: [Step] -> [Point]
simulate = ((1, 0, 0) :) . drop 1 . go (0, 0, 0)
  where
    go point [] = [point]
    go point (step : xs) =
        point : go (applyStep step point) xs

maxHeight :: [Point] -> Int
maxHeight = maximum . map (\(r, _, _) -> r)

part1 :: String -> Int
part1 = maxHeight . simulate . pInput

pointsBetween :: Point -> Point -> [Point]
pointsBetween (r1, c1, d1) (r2, c2, d2)
    | r1 < r2 = [(r, c1, d1) | r <- [r1 .. r2]]
    | r1 > r2 = [(r, c1, d1) | r <- [r2 .. r1]]
    | c1 < c2 = [(r1, c, d1) | c <- [c1 .. c2]]
    | c1 > c2 = [(r1, c, d1) | c <- [c2 .. c1]]
    | d1 < d2 = [(r1, c1, d) | d <- [d1 .. d2]]
    | otherwise = [(r1, c1, d) | d <- [d2 .. d1]]

uniqueBranchSegments :: [Point] -> Set Point
uniqueBranchSegments [] = S.empty
uniqueBranchSegments (x : xs) = go x xs
  where
    go _ [] = S.empty
    go current (y : ys) = S.union (S.fromList $ pointsBetween current y) $ go y ys

part2 :: String -> Int
part2 = S.size . foldr (S.union . uniqueBranchSegments . simulate . pInput) S.empty . lines


adjacentPoints :: Point -> [Point]
adjacentPoints (r, c, d) =
    [ (r + 1, c, d)
    , (r, c + 1, d)
    , (r, c - 1, d)
    , (r, c, d + 1)
    , (r, c, d - 1)
    , (r - 1, c, d)
    ]

validAdjacentPoints :: Set Point -> Point -> [Point]
validAdjacentPoints s = filter (`S.member` s) . adjacentPoints

distancesFrom :: Point -> Set Point -> Map Point Int
distancesFrom start validPoints = go M.empty [(start, 0)]
  where
    go mapping [] = mapping
    go mapping ((point, dist) : xs)
        | point `M.member` mapping = go mapping xs
        | otherwise = go newMapping (xs <> newStates)
      where
        newPoints = validAdjacentPoints validPoints point
        newStates = map (,1 + dist) newPoints
        newMapping = M.insert point dist mapping

part3 :: String -> Int
part3 s = minimum $ map distToAllLeavs mainTrunk
  where
    branches = map (simulate . pInput) $ lines s
    uniqueSegments = foldr (S.union . uniqueBranchSegments) S.empty branches
    leaves = map last branches
    distToAllLeavs point = sum $ map (distancesFrom point uniqueSegments M.!) leaves
    height = maximum $ map maxHeight branches
    mainTrunk = filter (`S.member` uniqueSegments) [(r, 0, 0) | r <- [1 .. height]]