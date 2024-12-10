{-# OPTIONS_GHC -Wno-x-partial #-}

module Quests.Q13 (run, part1, part2, part3) where

import Control.Arrow
import Data.Char (digitToInt, isDigit)
import Data.Heap qualified as H
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S

run :: IO ()
run = mempty

type Point = (Int, Int)

data Board = Board
    { bMap :: Map Point Int
    , bWidth :: Int
    , bHeight :: Int
    , bStarts :: [Point]
    , bEnd :: Point
    }
    deriving (Show, Eq, Ord)

makeBoard :: String -> Board
makeBoard s =
    Board
        { bMap = M.map f mapping
        , bWidth = width
        , bHeight = height
        , bStarts = starts
        , bEnd = end
        }
  where
    ls = lines s
    height = length ls
    width = length $ head ls
    processRow rowNum = filter ((/= '#') . snd) . zipWith (curry (first (rowNum,))) [0 ..]
    mapping = M.fromList . concatMap (uncurry processRow) $ zip [0 ..] ls
    starts = M.keys $ M.filter (== 'S') mapping
    end = head $ M.keys $ M.filter (== 'E') mapping
    f c
        | c == 'S' = 0
        | c == 'E' = 0
        | otherwise = digitToInt c

adjacentPoints :: Point -> [Point]
adjacentPoints (r, c) = [(r - 1, c), (r, c - 1), (r, c + 1), (r + 1, c)]

validAdjacentPoints :: Board -> Point -> [Point]
validAdjacentPoints b = filter (`M.member` bMap b) . adjacentPoints

minTimeBetweenLevels :: Int -> Int -> Int
minTimeBetweenLevels a b = min moveUp moveDown
  where
    moveUp = (b - a) `mod` 10
    moveDown = (a - b) `mod` 10

findShortestTime :: Board -> [Point] -> Point -> Maybe Int
findShortestTime b starts end = go (H.fromList $ map (H.Entry 0) starts) S.empty
  where
    mapping = bMap b
    go heap visited
        | H.null heap = Nothing
        | point `S.member` visited = go newHeap visited
        | point == end = Just time
        | otherwise = do
            let adjacent = validAdjacentPoints b point
                currentLevel = mapping M.! point
                additionalTime level1 level2 = succ $ minTimeBetweenLevels level1 level2
                getNewTime = (time +) . additionalTime currentLevel . (mapping M.!)
                newStates = H.fromList $ map (H.Entry <$> getNewTime <*> id) adjacent
            go (newHeap <> newStates) (S.insert point visited)
      where
        minEntry = H.minimum heap
        point = H.payload minEntry
        time = H.priority minEntry
        newHeap = H.deleteMin heap

part1 :: String -> Int
part1 s = fromJust $ findShortestTime b starts end
  where
    b = makeBoard s
    starts = bStarts b
    end = bEnd b

part2 :: String -> Int
part2 s = fromJust $ findShortestTime b starts end
  where
    b = makeBoard s
    starts = bStarts b
    end = bEnd b

part3 :: String -> Int
part3 s = fromJust $ findShortestTime b starts end
  where
    b = makeBoard s
    starts = bStarts b
    end = bEnd b
