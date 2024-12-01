{-# OPTIONS_GHC -Wno-x-partial #-}

module Quests.Q8 where

import Data.Text qualified as T

run :: IO ()
run = do
    let prefix = "./input/q8"
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

odds :: [Int]
odds = [1, 3 ..]

nthOdd :: Int -> Int
nthOdd = pred . (* 2)

part1 :: String -> Int
part1 s = diff * nthOdd numOdds
  where
    n = read s
    (numOdds, sumOdds) = head $ filter ((>= n) . snd) $ map ((,) <*> (^ (2 :: Int))) [1 ..]
    diff = sumOdds - n

part2 :: String -> Int
part2 s = finalWidth * (totalBlocks - 20240000)
  where
    priests = read s
    priestAcolytes = 1111
    go (width, numBlocks, thickness)
        | numBlocks >= 20240000 = (width, numBlocks, thickness)
        | otherwise =
            let newWidth = width + 2
                newThickness = (thickness * priests) `rem` priestAcolytes
                newNumBlocks = numBlocks + newWidth * newThickness
             in go (newWidth, newNumBlocks, newThickness)
    (finalWidth, totalBlocks, _) = go (1, 1, 1)

nextHighTowerStep :: Int -> Int -> (Int, ([Int], Int, Int)) -> (Int, ([Int], Int, Int))
nextHighTowerStep highPriests highPriestAcolytes (numBlocks, (xs, width, thickness)) =
    let newThickness = highPriestAcolytes + (thickness * highPriests) `rem` highPriestAcolytes
        newWidth = width + 2
        newNumBlocks = numBlocks + newThickness * newWidth
     in (newNumBlocks, (newThickness : map (+ newThickness) xs, newWidth, newThickness))

part3 :: String -> Int
part3 s = finalNumUsed - numAvailable
  where
    numAvailable :: Int
    numAvailable = 202400000
    highPriests = read s
    highPriestAcolytes = 10
    (_, (partialColumns, finalWidth, _)) = until ((>= numAvailable) . fst) (nextHighTowerStep highPriests highPriestAcolytes) (1, ([1], 1, 1))
    numToRemove = flip rem highPriestAcolytes . (highPriests * finalWidth *)
    correctHeight = (-) <*> numToRemove
    nonOuterColumns = drop 1 partialColumns
    partialCorrectColumns = map correctHeight nonOuterColumns
    finalNumUsed = (2 * head partialColumns) + sum (map (2 *) $ init partialCorrectColumns) + last partialCorrectColumns