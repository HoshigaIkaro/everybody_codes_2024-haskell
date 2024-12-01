{-# OPTIONS_GHC -Wno-x-partial #-}

module Quests.Q5 (run, part1, part2, part3) where

import Control.Monad (replicateM)
import Control.Monad.State
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T

run :: IO ()
run = do
  let prefix = "./input/q5"
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

type Column = [Int]

makeColumns :: String -> [Column]
makeColumns = transpose . map (map read . words) . lines

type RoundNumber = Int
type BoardState = ([Column], RoundNumber)
type ColIndex = Int
type RowIndex = Int

findNewPosition :: [Column] -> Int -> Int -> (RowIndex, ColIndex)
findNewPosition board column n
  | n <= length (board !! column) = (n - 1, column)
  | n <= 2 * length (board !! column) = (2 * length (board !! column) - n + 1, column)
  | otherwise = findNewPosition board column (mod n (2 * length (board !! column)))

insertAt :: (RowIndex, ColIndex) -> Int -> [Column] -> [Column]
insertAt (row, col) value originalBoard = zipWith (curry f) originalBoard [0 ..]
 where
  f (column, newCol)
    | col == newCol = left <> [value] <> right
    | otherwise = column
   where
    left = take row column
    right = drop row column

deleteAt :: (RowIndex, ColIndex) -> [Column] -> [Column]
deleteAt (row, col) originalBoard = zipWith (curry f) originalBoard [0 ..]
 where
  f (column, newCol)
    | col == newCol = concat $ [take row, drop (row + 1)] <*> pure column
    | otherwise = column

nextStep :: State BoardState Int
nextStep = do
  (board, originalColumn) <- get
  let newRoundNumber = succ originalColumn `rem` length board
      clapper = head (board !! originalColumn)
      originalPosition = (0 :: Int, originalColumn)
      newPosition = findNewPosition board newRoundNumber clapper
      newBoard = insertAt newPosition clapper $ deleteAt originalPosition board
  put (newBoard, newRoundNumber)
  return . read $ concatMap (show . (!! 0)) newBoard

doNumSteps :: Int -> State BoardState [Int]
doNumSteps n = replicateM n nextStep

initialState :: String -> ([Column], Int)
initialState s = (makeColumns s, 0)

part1 :: String -> Int
part1 = last . evalState (doNumSteps 10) . initialState

type Shouts = Map Int Int

succFor :: Int -> Shouts -> Shouts
succFor = M.alter f
 where
  f Nothing = Just 1
  f (Just value) = Just $ 1 + value

untilNumShouts :: Int -> State ([Column], Int, Shouts) Shouts
untilNumShouts n = go
 where
  go = do
    (board, originalColumn, shouts) <- get
    let newRoundNumber = succ originalColumn `rem` length board
        clapper = head (board !! originalColumn)
        originalPosition = (0 :: Int, originalColumn)
        newPosition = findNewPosition board newRoundNumber clapper
        newBoard = insertAt newPosition clapper $ deleteAt originalPosition board
        shouted = read $ concatMap (show . (!! 0)) newBoard
        newShouts = succFor shouted shouts
    put (newBoard, newRoundNumber, newShouts)
    if newShouts M.! shouted == n
      then return newShouts
      else go

calcResults :: Int -> Shouts -> Int
calcResults n shouts = key * total
 where
  total = sum $ M.elems shouts
  (key, _) = (!! 0) . M.toList $ M.filter (== n) shouts

part2 :: String -> Int
-- part2 = evalState (doNumSteps 10) . initialState
part2 s = calcResults n . evalState (untilNumShouts n) . (\(a, b) -> (a, b, M.empty)) $ initialState s
 where
  n = 2024
part3 :: String -> Int
part3 s = fst . M.findMax . evalState (untilNumShouts n) . (\(a, b) -> (a, b, M.empty)) $ initialState s
 where
  n = 2024
