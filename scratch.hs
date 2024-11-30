{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Quests.Q10 (run, part1, part2, part3) where

import Control.Arrow (Arrow (first))
import Data.Char (isAlpha, ord)
import Data.List (findIndex, group, groupBy, intercalate, intersect, permutations, sort, transpose, uncons, (\\))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T

run :: IO ()
run = mempty

type Point = (Int, Int)
data Board = Board
    { boardMapping :: Map Point Char
    , boardWidth :: Int
    , boardHeight :: Int
    }
    deriving (Eq)

instance Show Board where
    show = showBoard

makeBoard :: String -> Board
makeBoard s =
    Board
        { boardMapping = M.fromList . concatMap (uncurry processRow) $ zip [0 ..] ls
        , boardWidth = width
        , boardHeight = height
        }
  where
    ls = lines s
    height = length ls
    width = length $ head ls
    processRow rowNum = map (first (rowNum,)) . zip [0 ..]

getWithFilter :: (Char -> Bool) -> Map Point Char -> [Point] -> Maybe [Char]
getWithFilter filterFunc mapping = fmap (filter filterFunc) . sequence . map (flip M.lookup mapping)

getRow :: Int -> Int -> Board -> Maybe [Char]
getRow = getRowWithFilter isAlpha

getRowWithFilter :: (Char -> Bool) -> Int -> Int -> Board -> Maybe [Char]
getRowWithFilter filterFunc leftmostCol rowNum board = getWithFilter filterFunc (boardMapping board) [(rowNum, colNum) | colNum <- [leftmostCol .. leftmostCol + 7]]

getColumn :: Int -> Int -> Board -> Maybe [Char]
getColumn = getColWithFilter isAlpha

getColWithFilter :: (Char -> Bool) -> Int -> Int -> Board -> Maybe [Char]
getColWithFilter filterFunc topmostRow colNum board = getWithFilter filterFunc (boardMapping board) [(rowNum, colNum) | rowNum <- [topmostRow .. topmostRow + 7]]

emptyPoints :: Point -> Board -> [Point]
emptyPoints (topmostRow, leftmostCol) = map fst . M.toList . M.filterWithKey f . boardMapping
  where
    inbounds bound value = value > bound && value <= bound + 6
    f (row, col) char = char == '.' && inbounds topmostRow row && inbounds leftmostCol col

updateCharAt :: Point -> Point -> Maybe Board -> Maybe Board
updateCharAt (topmostRow, leftmostCol) point@(rowNum, colNum) maybeBoard = do
    board <- maybeBoard
    row <- getRow leftmostCol rowNum board
    col <- getColumn topmostRow colNum board
    (newChar, _) <- uncons $ row `intersect` col
    pure
        board
            { boardMapping = M.insert point newChar (boardMapping board)
            }

readRunic :: [Point] -> Board -> String
readRunic points board = map (mapping M.!) (sort points)
  where
    mapping = boardMapping board

boardStringToRunic :: String -> String
boardStringToRunic s = readRunic empty $ fromJust $ foldr (updateCharAt (0, 0)) (Just board) empty
  where
    board = makeBoard s
    empty = emptyPoints (0, 0) board

part1 :: String -> String
part1 = boardStringToRunic

basePower :: Char -> Int
basePower = (subtract (ord 'A' - 1)) . ord

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

partialSolve :: Point -> [Point] -> Board -> Board
partialSolve _ [] board = board
partialSolve startingPoint (p : ps) board = case updateCharAt startingPoint p (Just board) of
    Nothing -> partialSolve startingPoint ps board
    Just newBoard -> partialSolve startingPoint ps newBoard

findQuestionInRowCol :: Point -> Point -> String -> String -> Point
findQuestionInRowCol (topmostRow, leftmostCol) (currentRow, currentCol) row col
    | '?' `elem` row = (currentRow, leftmostCol + correctOffset (offsetIn row))
    | otherwise = (topmostRow + correctOffset (offsetIn col), currentCol)
  where
    offsetIn = fromJust . findIndex (== '?')
    correctOffset = id

-- correctOffset offset
--     | offset > 2 = offset + 4
--     | otherwise = offset

outerChars :: String -> String
outerChars = liftA2 (<>) (take 2) (drop 4)

innerChars :: String -> String
innerChars = drop 2 . take 6

updateCharAtWithUnknown :: Point -> Point -> Maybe Board -> Maybe Board
updateCharAtWithUnknown start@(topmostRow, leftmostCol) point@(rowNum, colNum) maybeBoard = do
    board <- maybeBoard
    let filterFunc = liftA2 (||) isAlpha (`elem` ['?', '.'])
    row <- getRowWithFilter (const True) leftmostCol rowNum board
    col <- getColWithFilter (const True) topmostRow colNum board
    let combined = row <> col
        outerRow = outerChars row
        outerCol = outerChars col
        innerRow = innerChars row
        innerCol = innerChars col
        processed = filter ((== 1) . length) . group $ sort $ filter (/= '.') combined
    -- _ <- if length processed /= 2 then Nothing else Just ()
    questionPoint <-
        if '?' `elem` combined
            then Just $ findQuestionInRowCol start point row col
            else Nothing
    let nonQuestionString = filter (/= '?') (filter (`elem` innerRow) outerRow) <> (filter (`elem` innerCol) outerCol)
    let newChar = head nonQuestionString
        withNonEmptyInsert = M.insert point newChar (boardMapping board)
        newMapping = M.insert questionPoint newChar withNonEmptyInsert
    pure
        board
            { boardMapping = newMapping
            }

maybeCompleteSolve :: Point -> Board -> Maybe Board
maybeCompleteSolve startingPoint board = do
    let points = emptyPoints startingPoint board
    newBoard <- foldr (updateCharAtWithUnknown startingPoint) (Just board) points
    pure newBoard

showBoard :: Board -> String
showBoard = unlines . map (map snd . sort) . groupBy f . M.toList . boardMapping
  where
    f ((r1, _), _) ((r2, _), _) = r1 == r2

parseCombinedBoard :: String -> ([Point], Board)
parseCombinedBoard s = (allStartingPoints, board)
  where
    board = makeBoard s
    calcOverlapping = flip div 6 . subtract 2
    numHorizontal = calcOverlapping $ boardWidth board
    numVertical = calcOverlapping $ boardHeight board
    allStartingPoints = [(rowNum * 6, colNum * 6) | rowNum <- [0 .. numVertical - 1], colNum <- [0 .. numHorizontal - 1]]

maybeSolve :: Point -> Board -> Maybe Board
maybeSolve start board = head $ map f (permutations positions)
  where
    positions = runePositions start
    f permutation = maybeCompleteSolve start $ partialSolve start permutation board

processInOrder :: [Point] -> ([Point], Board) -> ([Point], Board)
processInOrder [] result = result
processInOrder (p : ps) (solvedStartingPoints, board) =
    case maybeSolve p board of
        Just newBoard -> processInOrder ps (p : solvedStartingPoints, newBoard)
        Nothing -> processInOrder ps (solvedStartingPoints, board)

runePositions :: Point -> [Point]
runePositions (topmostRow, leftmostCol) = do
    rowDelta <- [topmostRow + 2 .. topmostRow + 5]
    colDelta <- [leftmostCol + 2 .. leftmostCol + 5]
    pure (rowDelta, colDelta)

processComplete :: [Point] -> ([Point], Board) -> ([Point], Board)
processComplete [] result = result
processComplete points initialState@(initialProcessed, initialBoard)
    | initialBoard == newBoard = initialState
    | otherwise = processComplete (points \\ processedPoints) (initialProcessed <> processedPoints, newBoard)
  where
    (processedPoints, newBoard) = processInOrder points initialState

-- part3 :: String -> Int
-- part3 s = sum $ map (effectivePower . flip readRunic finalBoard . runePositions) startingPoints
part3 s = finalBoard
  where
    (points, board) = parseCombinedBoard s
    (startingPoints, finalBoard) = processComplete points ([], board)
