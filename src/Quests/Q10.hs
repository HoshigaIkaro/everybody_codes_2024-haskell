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
showBoard = unlines . map (map snd . sort) . groupBy f . M.toList . bMap
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
    inbounds bound value = value > bound && value <= bound + 6
    f (row, col) char = char == '.' && inbounds topmostRow row && inbounds leftmostCol col

updateCharAt :: StartPoint -> Point -> Maybe Board -> Maybe Board
updateCharAt (topmostRow, leftmostCol) point@(rowNum, colNum) maybeBoard = do
    board <- maybeBoard
    row <- getRow leftmostCol rowNum board
    col <- getColumn topmostRow colNum board
    (newChar, _) <- uncons $ row `intersect` col
    pure
        board
            { bMap = M.insert point newChar (bMap board)
            }

readRunic :: [Point] -> Board -> String
readRunic points board = map (mapping M.!) (sort points)
  where
    mapping = bMap board

boardStringToRunic :: String -> String
boardStringToRunic s = readRunic empty $ partialSolve (0, 0) empty board
  where
    board = makeBoard s
    empty = emptyPoints (0, 0) board

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

partialSolve :: StartPoint -> [Point] -> Board -> Board
partialSolve _ [] board = board
partialSolve startingPoint (p : ps) board = case updateCharAt startingPoint p (Just board) of
    Nothing -> partialSolve startingPoint ps board
    Just newBoard -> partialSolve startingPoint ps newBoard

findQuestionInRowCol :: StartPoint -> Point -> String -> String -> Point
findQuestionInRowCol (topmostRow, leftmostCol) (currentRow, currentCol) row col
    | '?' `elem` row = (currentRow, leftmostCol + correctOffset (offsetIn row))
    | otherwise = (topmostRow + correctOffset (offsetIn col), currentCol)
  where
    offsetIn = fromJust . elemIndex '?'
    correctOffset = id

outerChars :: String -> String
outerChars = liftA2 (<>) (take 2) (drop 6)

innerChars :: String -> String
innerChars = drop 2 . take 6

updateCharAtWithUnknown :: StartPoint -> Point -> Maybe Board -> Maybe Board
updateCharAtWithUnknown start@(topmostRow, leftmostCol) point@(rowNum, colNum) maybeBoard = do
    board <- maybeBoard
    -- let filterFunc = liftA2 (||) isAlpha (`elem` ['?', '.'])
    row <- getRowWithFilter (const True) leftmostCol rowNum board
    col <- getColWithFilter (const True) topmostRow colNum board
    let combined = row <> col
        -- processed = filter ((== 1) . length) . group $ sort $ filter (/= '.') combined
        outerRow = outerChars row
        outerCol = outerChars col
        innerRow = innerChars row
        innerCol = innerChars col
    -- _ <- if length processed /= 2 then Nothing else Just ()
    -- (nonQuestionString, _) <- uncons $ filter (/= "?") processed
    questionPoint <-
        if '?' `elem` combined
            then Just $ findQuestionInRowCol start point row col
            else Nothing
    let nonQuestionString = filter (/= '?') (filter (`notElem` innerRow) outerRow) <> filter (`notElem` innerCol) outerCol
    let newChar = head nonQuestionString
        withNonEmptyInsert = M.insert point newChar (bMap board)
        newMapping = M.insert questionPoint newChar withNonEmptyInsert
    pure
        board
            { bMap = newMapping
            }

maybeCompleteSolve :: StartPoint -> Board -> Maybe Board
maybeCompleteSolve startingPoint board = do
    let points = emptyPoints startingPoint board
    foldr (updateCharAtWithUnknown startingPoint) (Just board) points

runePositions :: StartPoint -> [Point]
runePositions (topmostRow, leftmostCol) = do
    rowDelta <- [topmostRow + 2 .. topmostRow + 5]
    colDelta <- [leftmostCol + 2 .. leftmostCol + 5]
    pure (rowDelta, colDelta)

maybeSolve :: Point -> Board -> Maybe Board
maybeSolve start board = maybeCompleteSolve start $ partialSolve start (runePositions start) board

-- positions = runePositions start
-- f permutation = maybeCompleteSolve start $ partialSolve start permutation board

processInOrder :: [StartPoint] -> ([StartPoint], Board) -> ([StartPoint], Board)
processInOrder [] result = result
processInOrder (p : ps) (solvedStartingPoints, board) =
    case maybeSolve p board of
        Just newBoard -> processInOrder ps (p : solvedStartingPoints, newBoard)
        Nothing -> processInOrder ps (solvedStartingPoints, board)

processComplete :: [StartPoint] -> ([StartPoint], Board) -> ([StartPoint], Board)
processComplete [] result = result
processComplete points initialState@(initialProcessed, initialBoard)
    | initialBoard == newBoard = (processedPoints, newBoard)
    | otherwise = processComplete (points \\ processedPoints) (initialProcessed <> processedPoints, newBoard)
  where
    (processedPoints, newBoard) = processInOrder points initialState

part3 :: String -> Int
part3 s = sum $ map (effectivePower . flip readRunic finalBoard . runePositions) finalStartingPoints
  where
    -- part3 s = finalBoard
    (points, board) = parseCombinedBoard s
    (finalStartingPoints, finalBoard) = processComplete points ([], board)
