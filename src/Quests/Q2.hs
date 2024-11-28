{-# LANGUAGE OverloadedStrings #-}

module Quests.Q2 (run, part1, part2, part3)
where

import Data.Function (on)
import Data.List (maximumBy)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T

run :: IO ()
run = do
  let prefix = "./input/q2/"
      firstInputName = prefix <> "p1.txt"
      secondInputName = prefix <> "p2.txt"
      thirdInputName = prefix <> "p3.txt"
      trim = (T.strip . T.pack)
  firstInput <- trim <$> readFile firstInputName
  secondInput <- trim <$> readFile secondInputName
  thirdInput <- trim <$> readFile thirdInputName
  print $ part1 firstInput
  print $ part2 secondInput
  print $ part3 thirdInput

toWordList :: Text -> [Text]
toWordList = T.split (== ',') . T.drop 6

wordListAddReverse :: [T.Text] -> [Text]
wordListAddReverse wordlist = S.toList . S.fromList $ wordlist <> (map T.reverse wordlist)

part1 :: Text -> Int
part1 s = sum $ map (flip T.count text) wordList
 where
  ls = T.lines s
  wordList = toWordList $ ls !! 0
  text = ls !! 2

----

symbolsInLine :: [Text] -> Text -> Int
symbolsInLine _ "" = 0
symbolsInLine wordlist line = go line 0
 where
  prefixwords str = filter (flip T.isPrefixOf str) wordlist
  bestword = maximumBy (compare `on` T.length) . prefixwords
  go :: Text -> Int -> Int
  go "" n = n
  go str 0
    | prefixwords str == [] = go (T.drop 1 str) 0
    | otherwise = succ . go (T.drop 1 str) . pred . T.length $ bestword str
  go str n
    | prefixwords str == [] = 1 + go (T.drop 1 str) (n - 1)
    | otherwise =
        let newLen = T.length $ bestword str
            extra = max 0 (newLen - n)
         in 1 + go (T.drop 1 str) (n + extra - 1)

part2 :: Text -> Int
part2 s = sum . map handleLine $ drop 2 ls
 where
  ls = T.lines s
  wordlist = wordListAddReverse $ toWordList $ ls !! 0
  handleLine line = symbolsInLine wordlist line

----

type Point = (Int, Int)
type PositionChar = (Point, Char)

type Board = Map Point Char

data Direction = UpDir | DownDir | LeftDir | RightDir deriving (Show)

newWordPositionWith :: Point -> String -> Direction -> [PositionChar]
newWordPositionWith (row, col) word dir =
  case dir of
    UpDir -> map (\(char, delta) -> ((row - delta, col), char)) $ zip word [0 ..]
    DownDir -> map (\(char, delta) -> ((row + delta, col), char)) $ zip word [0 ..]
    LeftDir -> map (\(char, delta) -> ((row, col - delta), char)) $ zip word [0 ..]
    RightDir -> map (\(char, delta) -> ((row, col + delta), char)) $ zip word [0 ..]
maybeWrapRightToLeft :: Point -> (Point, Char) -> Maybe (Point, Char)
maybeWrapRightToLeft (height, width) ((row, col), char)
  | row >= height || row < 0 = Nothing
  | otherwise = Just ((row, col `mod` width), char)

maybeWrappedDirections :: Point -> [PositionChar] -> Maybe [PositionChar]
maybeWrappedDirections size positions =
  if length wrapped == length positions
    then Just wrapped
    else Nothing
 where
  wrapped = mapMaybe (maybeWrapRightToLeft size) positions

allPositionVectorsForWord :: Point -> Point -> String -> [[PositionChar]]
allPositionVectorsForWord size point word = mapMaybe processDirection [UpDir, DownDir, LeftDir, RightDir]
 where
  processDirection = maybeWrappedDirections size . newWordPositionWith point word

genAllWordsNewCharPositions :: Point -> Point -> [String] -> [[PositionChar]]
genAllWordsNewCharPositions size point = concatMap (allPositionVectorsForWord size point)

newVecWorks :: Board -> [PositionChar] -> Bool
newVecWorks board = all f
 where
  f (point, char) = case M.lookup point board of
    Just value -> char == value
    Nothing -> False

updateAtPoint :: Point -> [String] -> Board -> Point -> Set Point -> Set Point
updateAtPoint size wordlist board point set = foldr f set newPoints
 where
  newPoints = genAllWordsNewCharPositions size point wordlist
  f newPositions accSet
    | newVecWorks board newPositions = accSet `S.union` S.fromList (map fst newPositions)
    | otherwise = accSet

processBoard :: Board -> Point -> [String] -> Set Point
processBoard board size@(height, width) wordlist = foldr (updateAtPoint size wordlist board) S.empty allPoints
 where
  allPoints = [(row, col) | row <- [0 .. height], col <- [0 .. width]]

part3 :: Text -> Int
part3 s = S.size $ processBoard board (height, width) wordlist
 where
  ls = T.lines s
  wordlist = map T.unpack $ toWordList $ ls !! 0
  boardLines = map T.unpack $ drop 2 ls
  height = length boardLines
  width = length $ boardLines !! 0
  f (row, rowNum) =
    zipWith
      (curry (\(col, colNum) -> ((rowNum, colNum), col)))
      row
      [0 ..]
  board :: Map Point Char
  board = M.fromList $ concatMap f $ zip boardLines [0 ..]