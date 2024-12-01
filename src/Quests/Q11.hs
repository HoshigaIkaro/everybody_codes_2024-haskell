{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Quests.Q11 (run, part1, part2, part3) where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T

run :: IO ()
run = do
    let prefix = "./input/q11"
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

type Category = Text
type NextCategoryMap = Map Category [Category]
type Populations = Map Category Int

populationSize :: Populations -> Int
populationSize = sum . M.elems

parseLine :: String -> (Category, [Category])
parseLine s = (start, categories)
  where
    sides = T.split (== ':') (T.pack s)
    start = head sides
    categories = T.split (== ',') (last sides)

parseCategories :: String -> NextCategoryMap
parseCategories = M.fromList . map parseLine . lines

updateFor :: NextCategoryMap -> (Category, Int) -> Populations -> Populations
updateFor mapping (old, amount) population = foldr (M.alter f) population (mapping M.! old)
  where
    f = Just . maybe amount (amount +)

populationAfter :: Int -> NextCategoryMap -> Populations -> Populations
populationAfter 0 _ population = population
populationAfter n mapping population = populationAfter (n - 1) mapping newPopulation
  where
    newPopulation = foldr (updateFor mapping) M.empty (M.toList population)

part1 :: String -> Int
part1 = populationSize . flip (populationAfter 4) (M.singleton "A" 1) . parseCategories

part2 :: String -> Int
part2 = populationSize . flip (populationAfter 10) (M.singleton "Z" 1) . parseCategories

-- | Assuming input non-negative
maxMin :: [Int] -> (Int, Int)
maxMin = foldr f (minBound, maxBound)
  where
    f newVal (maxVal, minVal)
        | newVal < minVal = (maxVal, newVal)
        | newVal > maxVal = (newVal, minVal)
        | otherwise = (maxVal, minVal)

part3 :: String -> Int
part3 s = uncurry (-) . maxMin $ simulatedAllTypes
  where
    mapping = parseCategories s
    simulatedAllTypes = map (populationSize . populationAfter 20 mapping . flip M.singleton 1) (M.keys mapping)