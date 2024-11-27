module Quests.Q1 (run, part1, part2, part3) where

import Data.Text qualified as T

run :: IO ()
run = do
    let prefix = "./input/q1/"
        firstInputName = prefix <> "p1.txt"
        secondInputName = prefix <> "p2.txt"
        thirdInputName = prefix <> "p3.txt"
        trim = (T.unpack . T.strip . T.pack)
    firstInput <- trim <$> readFile firstInputName
    secondInput <- trim <$> readFile secondInputName
    thirdInput <- trim <$> readFile thirdInputName
    print $ part1 firstInput
    print $ part2 secondInput
    print $ part3 thirdInput

part1 :: String -> Int
part1 = sum . map potionsV1

potionsV1 :: Char -> Int
potionsV1 'A' = 0
potionsV1 'B' = 1
potionsV1 'C' = 3
potionsV1 _ = -1

part2 :: String -> Int
part2 = sum . filter (> 0) . map sum . chunksOf 2 . map potionsV2

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs
    | n <= length xs = take n xs : chunksOf n (drop n xs)
    | otherwise = []

potionsV2 :: Char -> Int
potionsV2 'A' = 1
potionsV2 'B' = 2
potionsV2 'C' = 4
potionsV2 'D' = 6
potionsV2 'x' = -1
potionsV2 _ = -1

part3 :: String -> Int
part3 = sum . map (creaturesToPotion . filter (/= 'x')) . chunksOf 3
  where
    creaturesToPotion [] = 0
    creaturesToPotion [a] = potionsV3 a - 2
    creaturesToPotion creatures@[_, _] = (flip (-) 2) . sum $ map potionsV3 creatures
    creaturesToPotion creatures = sum $ map potionsV3 creatures

potionsV3 :: Char -> Int
potionsV3 'A' = 2
potionsV3 'B' = 3
potionsV3 'C' = 5
potionsV3 'D' = 7
potionsV3 'x' = -1
potionsV3 _ = -1