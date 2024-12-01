module Quests.Q4 (run, part1, part2, part3) where

import Data.Text qualified as T

run :: IO ()
run = do
    let prefix = "./input/q4/"
        firstInputName = prefix <> "p1.txt"
        secondInputName = prefix <> "p2.txt"
        thirdInputName = prefix <> "p3.txt"
        trim = T.unpack . T.strip . T.pack
    firstInput <- trim <$> readFile firstInputName
    secondInput <- trim <$> readFile secondInputName
    thirdInput <- trim <$> readFile thirdInputName
    print $ part1 firstInput
    print $ part2 secondInput
    print $ part3 thirdInput

toNails :: String -> [Int]
toNails = map read . lines

calcMinStrikes :: [Int] -> Int
calcMinStrikes nails = sum $ map diff nails
  where
    minLength = minimum nails
    diff len = len - minLength

part1 :: String -> Int
part1 = calcMinStrikes . toNails

part2 :: String -> Int
part2 = calcMinStrikes . toNails

calcStrikes :: [Int] -> Int -> Int
calcStrikes nails target = sum $ map absDiff nails
  where
    absDiff nail = abs (target - nail)

part3 :: String -> Int
part3 s = minimum $ map (calcStrikes nails) nails
  where
    nails = toNails s