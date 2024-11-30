{-# OPTIONS_GHC -Wno-x-partial #-}

module Quests.Q9 where

import Control.Arrow (Arrow ((***)))
import Control.Monad (replicateM_)
import Control.Monad.State (MonadState (get), State, execState, put)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Map (Map)
import Data.Map qualified as M

run :: IO ()
run = mempty

applyStamps :: [Int] -> Int -> Int
applyStamps [] _ = 0
applyStamps (x : xs) n = div n x + applyStamps xs (mod n x)

part1 :: String -> Int
part1 = sum . map (applyStamps stamps . read) . lines
  where
    stamps = reverse [1, 3, 5, 10]

type ComputeState = (Map Int Int, Int)

nextComputeState :: [Int] -> State ComputeState ()
nextComputeState stamps = do
    (mapping, n) <- get
    let newVal = minimum . map ((+ 1) . (mapping M.!) . (n -)) $ filter (<= n) stamps
    put (M.insert n newVal mapping, succ n)

computeStateUntil :: Int -> [Int] -> State ComputeState ()
computeStateUntil num stamps = replicateM_ num (nextComputeState stamps)

computedFor :: Int -> [Int] -> Map Int Int
computedFor num stamps = fst $ execState (computeStateUntil num stamps) (M.singleton 0 0, 1)

part2 :: String -> Int
part2 s = sum $ map (computed M.!) values
  where
    values = map read $ lines s
    stamps = [1, 3, 5, 10, 15, 16, 20, 24, 25, 30]
    computed = computedFor (maximum values) stamps

validPairs :: Int -> [(Int, Int)]
validPairs n = map (\a -> (a, n - a)) [mid - 49 .. mid + 49]
  where
    mid = n `div` 2

bestPair :: Map Int Int -> Int -> (Int, Int)
bestPair computedStamps n = minimumBy (compare `on` uncurry (+)) . map (f *** f) $ validPairs n
  where
    f = (computedStamps M.!)

part3 :: String -> Int
part3 s = sum . map (uncurry (+) . bestPair computed) $ values
  where
    values = map read $ lines s
    stamps = [1, 3, 5, 10, 15, 16, 20, 24, 25, 30, 37, 38, 49, 50, 74, 75, 100, 101]
    computed = computedFor (maximum values) stamps
