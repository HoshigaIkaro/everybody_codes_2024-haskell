{-# OPTIONS_GHC -Wno-x-partial #-}

module Quests.Q15 (run, part1, part2, part3) where

import Control.Arrow
import Data.Char (isAlpha, ord)
import Data.Heap qualified as H
import Data.Ix
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T

run :: IO ()
run = do
    let prefix = "./input/q15"
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

type Point = (Int, Int)

data Board = Board
    { bMap :: Map Point Char
    , bWidth :: Int
    , bHeight :: Int
    , bFruits :: Set Char
    , bFruitLocations :: Map Point Char
    }
    deriving (Show, Eq, Ord)

showBoard :: Board -> String
showBoard b = unlines $ map processRow [0 .. height - 1]
  where
    mapping = bMap b
    width = bWidth b
    height = bHeight b
    processRow rowNum = [fromMaybe '#' (M.lookup (rowNum, c) mapping) | c <- [0 .. width - 1]]

makeBoard :: String -> Board
makeBoard s =
    Board
        { bMap = mapping
        , bWidth = width
        , bHeight = height
        , bFruits = fruits
        , bFruitLocations = M.filter isAlpha mapping
        }
  where
    ls = lines s
    height = length ls
    width = length $ head ls
    processRow rowNum = filter ((`notElem` "#~") . snd) . zipWith (curry (first (rowNum,))) [0 ..]
    mapping = M.fromList . concatMap (uncurry processRow) $ zip [0 ..] ls
    fruits = S.fromList $ M.elems $ M.filter isAlpha mapping

adjacentPoints :: Point -> [Point]
adjacentPoints (row, col) =
    [ (row - 1, col)
    , (row + 1, col)
    , (row, col - 1)
    , (row, col + 1)
    ]

validAdjacentPoints :: Map Point Char -> Point -> [Point]
validAdjacentPoints mapping = filter (`M.member` mapping) . adjacentPoints

findPathLengthsToFruits :: Board -> [Int]
findPathLengthsToFruits b = go [start] S.empty S.empty
  where
    start = (,0) . head $ filter ((== 0) . fst) $ M.keys mapping
    mapping = bMap b
    fruits = bFruits b
    go [] _ _ = []
    go ((point, dist) : xs) visited fruitsVisited
        | point `S.member` visited = go xs visited fruitsVisited
        | fruitsVisited == fruits = []
        | currentChar `S.member` fruits && currentChar `S.notMember` fruitsVisited = dist : rest
        | otherwise = rest
      where
        currentChar = mapping M.! point
        newVisited = S.insert point visited
        newFruitsVisited = S.insert currentChar fruitsVisited
        newStates = map (,succ dist) $ validAdjacentPoints mapping point
        rest = go (xs <> newStates) newVisited newFruitsVisited

part1 :: String -> Int
part1 s = sum . map (2 *) . findPathLengthsToFruits $ makeBoard s

toVisited :: Char -> Int
toVisited = (10 ^) . subtract (ord 'A') . ord

part2 :: String -> Int
part2 s = shortestCycleV2 b start
  where
    b = makeBoard s
    mapping = bMap b
    start = head $ filter ((== 0) . fst) $ M.keys mapping

splitToThreeBoards :: Board -> (Board, Board, Board)
splitToThreeBoards b = (one, two, three)
  where
    mapping = bMap b
    f :: Int -> Int -> Int -> Bool
    f minCol maxCol = inRange (minCol, maxCol)
    oneMapping = M.filterWithKey (const . f 0 83 . snd) mapping
    oneFruits = S.fromList $ M.elems $ M.filter isAlpha oneMapping
    oneFruitLocations = M.filter isAlpha oneMapping
    one = b{bMap = oneMapping, bFruits = oneFruits, bFruitLocations = oneFruitLocations, bWidth = 84}
    twoMapping = M.filterWithKey (const . f 83 171 . snd) mapping
    twoFruits = S.fromList $ M.elems $ M.filter isAlpha twoMapping
    twoFruitLocations = M.filter isAlpha twoMapping
    two = b{bMap = twoMapping, bFruits = twoFruits, bFruitLocations = twoFruitLocations, bWidth = 89}
    threeMapping = M.filterWithKey (const . f 171 255 . snd) mapping
    threeFruits = S.fromList $ M.elems $ M.filter isAlpha threeMapping
    threeFruitLocations = M.filter isAlpha threeMapping
    three = b{bMap = threeMapping, bFruits = threeFruits, bFruitLocations = threeFruitLocations, bWidth = 84}

shortestCycleV2 :: Board -> Point -> Int
shortestCycleV2 b start = go initialHeap S.empty
  where
    mapping = bMap b
    fruits = bFruits b
    initialHeap = H.singleton (H.Entry 0 (start, S.empty))
    go heap visited
        | H.null heap = 0
        | state `S.member` visited = go newHeap visited
        | point == start && visitedFruits == fruits = dist
        | isAlpha currentChar = go (newHeap <> newStatesUpdateFruit) newVisited
        | otherwise = go (newHeap <> newStates) newVisited
      where
        currentChar = mapping M.! point
        minState = H.minimum heap
        dist = H.priority minState
        state@(point, visitedFruits) = H.payload minState
        newHeap = H.deleteMin heap
        newPoints = validAdjacentPoints mapping point
        newStates = H.fromList $ map (H.Entry (succ dist) . (,visitedFruits)) newPoints
        newStatesUpdateFruit = H.fromList $ map (H.Entry (succ dist) . (,S.insert currentChar visitedFruits)) newPoints
        newVisited = S.insert state visited

solveLeft :: Board -> Int
solveLeft b = shortestCycleV2 b start
  where
    start = fst . maximum $ M.toList (bFruitLocations b)

solveRight :: Board -> Int
solveRight b = shortestCycleV2 b start
  where
    height = bHeight b
    start = fst . minimum $ filter ((== height - 2) . fst . fst) $ M.toList (bFruitLocations b)

solveMiddle :: Board -> Int
solveMiddle b = shortestCycleV2 b start
  where
    mapping = bMap b
    start = head $ filter ((== 0) . fst) $ M.keys mapping

part3 :: String -> Int
part3 s = solveLeft one + solveMiddle two + solveRight three
  where
    b = makeBoard s
    (one, two, three) = splitToThreeBoards b