module Quests.Q3 (run, part1, part2, part3) where

import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T

run :: IO ()
run = do
    let prefix = "./input/q3/"
        firstInputName = prefix <> "p1.txt"
        secondInputName = prefix <> "p2.txt"
        thirdInputName = prefix <> "p3.txt"
        trim = (T.unpack . T.strip . T.pack)
    firstInput <- trim <$> readFile firstInputName
    secondInput <- trim <$> readFile secondInputName
    thirdInput <- trim <$> readFile thirdInputName
    print $ part1 firstInput
    print $ part2 secondInput
    print $ part3 thirdInput

type Point = (Int, Int)
type Size = (Int, Int)

allPoints :: Size -> [Point]
allPoints (height, width) = concatMap (\row -> map ((,) row) [0 .. width - 1]) [0 .. height - 1]

makeBoard :: [String] -> Set Point
makeBoard original = S.fromList $ mapMaybe checkEarth $ zip (concat original) (allPoints (height, width))
  where
    height = length original
    width = length $ original !! 0
    checkEarth :: (Char, Point) -> Maybe Point
    checkEarth ('#', point) = Just point
    checkEarth _ = Nothing

adjacentPointsV1 :: Point -> [Point]
adjacentPointsV1 (row, col) =
    [ (row - 1, col)
    , (row + 1, col)
    , (row, col - 1)
    , (row, col + 1)
    ]

safe :: Set Point -> (Point -> [Point]) -> Point -> Bool
safe board adjacent = all (`S.member` board) . adjacent

runRemoveV1 :: Set Point -> Int
runRemoveV1 board
    | S.size board == 0 = 0
    | otherwise = S.size board + runRemoveV1 newBoard
  where
    newBoard = S.filter (safe board adjacentPointsV1) board

part1 :: String -> Int
part1 = runRemoveV1 . makeBoard . lines

part2 :: String -> Int
part2 = runRemoveV1 . makeBoard . lines

adjacentPointsV2 :: Point -> [Point]
adjacentPointsV2 (row, col) =
    [ (row - 1, col - 1)
    , (row - 1, col)
    , (row - 1, col + 1)
    , (row, col - 1)
    , (row, col + 1)
    , (row + 1, col - 1)
    , (row + 1, col)
    , (row + 1, col + 1)
    ]

runRemoveV2 :: Set Point -> Int
runRemoveV2 board
    | S.size board == 0 = 0
    | otherwise = S.size board + runRemoveV2 newBoard
  where
    newBoard = S.filter (safe board adjacentPointsV2) board

part3 :: String -> Int
part3 = runRemoveV2 . makeBoard . lines