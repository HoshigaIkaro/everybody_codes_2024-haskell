{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Quests.Q10 (run, part1, part2, part3) where

import Control.Arrow (Arrow (first))
import Data.Char (isAlpha, ord)
import Data.List (elemIndex, groupBy, intersect, sort, transpose, uncons, (\\))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T

run :: IO ()
run = mempty

type Point = (Int, Int)
type RowIndex = Int
type ColIndex = Int
type StartPoint = (Int, Int)

data Board = Board
    { bMap :: Map Point Char
    , bwidth :: Int
    , bheight :: Int
    }
    deriving (Eq)

showBoard :: Board -> String
showBoard = unlines . map (map snd) . groupBy f . sort . M.toList . bMap
  where
    f ((r1, _), _) ((r2, _), _) = r1 == r2

instance Show Board where
    show = showBoard

makeBoard :: String -> Board
makeBoard s =
    Board
        { bMap = M.fromList . concatMap (uncurry processRow) $ zip [0 ..] ls
        , bwidth = width
        , bheight = height
        }
  where
    ls = lines s
    height = length ls
    width = length $ head ls
    processRow rowNum = zipWith (curry (first (rowNum,))) [0 ..]

-- | Get all rune positions given the top left character's position
runePositions :: StartPoint -> [Point]
runePositions (topmostRow, leftmostCol) = do
    row <- [topmostRow + 2 .. topmostRow + 5]
    col <- [leftmostCol + 2 .. leftmostCol + 5]
    pure (row, col)

getFiltering :: (Char -> Bool) -> Map Point Char -> [Point] -> Maybe [Char]
getFiltering f mapping = fmap (filter f) . mapM (`M.lookup` mapping)

getRow :: ColIndex -> RowIndex -> Board -> Maybe [Char]
getRow = getRowWithFilter isAlpha

getRowWithFilter :: (Char -> Bool) -> ColIndex -> RowIndex -> Board -> Maybe [Char]
getRowWithFilter filterFunc leftBound rowNum board =
    getFiltering
        filterFunc
        (bMap board)
        [(rowNum, colNum) | colNum <- [leftBound .. leftBound + 7]]

getColumn :: RowIndex -> ColIndex -> Board -> Maybe [Char]
getColumn = getColWithFilter isAlpha

getColWithFilter :: (Char -> Bool) -> RowIndex -> ColIndex -> Board -> Maybe [Char]
getColWithFilter filterFunc topBound colNum board =
    getFiltering
        filterFunc
        (bMap board)
        [(rowNum, colNum) | rowNum <- [topBound .. topBound + 7]]

emptyPoints :: StartPoint -> Board -> [Point]
emptyPoints (topmostRow, leftmostCol) = map fst . M.toList . M.filterWithKey f . bMap
  where
    inbounds bound value = value > bound + 1 && value < bound + 6
    f (row, col) char = char == '.' && inbounds topmostRow row && inbounds leftmostCol col

updateCharAt :: StartPoint -> Point -> Maybe Board -> Maybe Board
updateCharAt (topmostRow, leftmostCol) point@(rowNum, colNum) maybeBoard = do
    board <- maybeBoard
    row <- getRow leftmostCol rowNum board
    col <- getColumn topmostRow colNum board
    (newChar, _) <- uncons $ row `intersect` col
    pure board{bMap = M.insert point newChar (bMap board)}

getRunic :: [Point] -> Board -> String
getRunic points board = map (mapping M.!) (sort points)
  where
    mapping = bMap board

-- | Solve standard block and produce the runic word
boardStringToRunic :: String -> String
boardStringToRunic = getRunic points . partialSolve (0, 0) points . makeBoard
  where
    points = runePositions (0, 0)

part1 :: String -> String
part1 = boardStringToRunic

basePower :: Char -> Int
basePower = subtract (ord 'A' - 1) . ord

effectivePower :: String -> Int
effectivePower = sum . zipWith (*) [1 ..] . map basePower

splitRowOfBoardStrings :: String -> [String]
splitRowOfBoardStrings = map unlines . transpose . map words . lines

part2 :: String -> Int
part2 =
    sum
        . map (effectivePower . boardStringToRunic)
        . concatMap (splitRowOfBoardStrings . T.unpack)
        . T.splitOn "\n\n"
        . T.pack

parseCombinedBoard :: String -> ([StartPoint], Board)
parseCombinedBoard s = (allStartingPoints, board)
  where
    board = makeBoard s
    calcOverlapping = flip div 6 . subtract 2
    numHorizontal = calcOverlapping $ bwidth board
    numVertical = calcOverlapping $ bheight board
    allStartingPoints = [(rowNum * 6, colNum * 6) | rowNum <- [0 .. numVertical - 1], colNum <- [0 .. numHorizontal - 1]]

-- | Solve all positions without considering '?'
partialSolve :: StartPoint -> [Point] -> Board -> Board
partialSolve _ [] board = board
partialSolve startingPoint (p : ps) board = case updateCharAt startingPoint p (Just board) of
    Just newBoard -> partialSolve startingPoint ps newBoard
    Nothing -> partialSolve startingPoint ps board

outerChars :: String -> String
outerChars = liftA2 (<>) (take 2) (drop 6)

innerChars :: String -> String
innerChars = drop 2 . take 6

findQuestionPointAndChar :: StartPoint -> Point -> String -> String -> Maybe (Char, Point)
findQuestionPointAndChar (topmostRow, leftmostCol) (currentRow, currentCol) row col
    | S.size diff /= 2 = Nothing
    | '?' `elem` row = Just (newChar, (currentRow, leftmostCol + offsetIn row))
    | otherwise = Just (newChar, (topmostRow + offsetIn col, currentCol))
  where
    offsetIn = fromJust . elemIndex '?'
    outerSet = S.fromList (outerChars row <> outerChars col)
    innerSet = S.fromList (innerChars row <> innerChars col)
    diff = outerSet S.\\ innerSet
    newChar = head . filter (/= '?') $ S.toList diff

updateCharAtWithUnknown :: StartPoint -> Point -> Maybe Board -> Maybe Board
updateCharAtWithUnknown start@(topmostRow, leftmostCol) point@(rowNum, colNum) maybeBoard = do
    board <- maybeBoard
    row <- getRowWithFilter (const True) leftmostCol rowNum board
    col <- getColWithFilter (const True) topmostRow colNum board
    (newChar, questionPoint) <- findQuestionPointAndChar start point row col
    let withNonEmptyInsert = M.insert point newChar (bMap board)
        newMapping = M.insert questionPoint newChar withNonEmptyInsert
    pure board{bMap = newMapping}

-- | Try to completely solve a partially solved block
maybeCompleteSolveAt :: StartPoint -> Board -> Maybe Board
maybeCompleteSolveAt startingPoint board = foldr (updateCharAtWithUnknown startingPoint) (Just board) points
  where
    points = emptyPoints startingPoint board

-- | Try to solve a block at the provided position in two passes
maybeSolve :: StartPoint -> Board -> Maybe Board
maybeSolve start = maybeCompleteSolveAt start . partialSolve start (runePositions start)

processInOrder :: [StartPoint] -> ([StartPoint], Board) -> ([StartPoint], Board)
processInOrder [] result = result
processInOrder (p : ps) (solvedStartingPoints, board) =
    case maybeSolve p board of
        Just newBoard -> processInOrder ps (p : solvedStartingPoints, newBoard)
        Nothing -> processInOrder ps (solvedStartingPoints, board)

processComplete :: [StartPoint] -> ([StartPoint], Board) -> ([StartPoint], Board)
processComplete [] result = result
processComplete points initialState@(_, initialBoard)
    | initialBoard == newBoard = newState
    | otherwise = processComplete (points \\ processedPoints) newState
  where
    newState@(processedPoints, newBoard) = processInOrder points initialState

part3 :: String -> Int
part3 s = sum $ map (effectivePower . flip getRunic finalBoard . runePositions) finalPoints
  where
    (points, board) = parseCombinedBoard s
    (finalPoints, finalBoard) = processComplete points ([], board)
