{-# LANGUAGE OverloadedStrings #-}

module Quests.Q6 (run, part1, part2, part3) where

import Data.Function (on)
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T

run :: IO ()
run = do
    let prefix = "./input/q6"
        firstInputName = prefix <> "/p1.txt"
        secondInputName = prefix <> "/p2.txt"
        thirdInputName = prefix <> "/p3.txt"
        trim = (T.strip . T.pack)
    firstInput <- trim <$> readFile firstInputName
    secondInput <- trim <$> readFile secondInputName
    thirdInput <- trim <$> readFile thirdInputName
    print $ part1 firstInput
    print $ part2 secondInput
    print $ part3 thirdInput

type Mapping = Map Text [Text]

updateMapping :: Text -> Mapping -> [Text] -> Mapping
updateMapping node = foldr f
  where
    f = M.insertWith (<>) node . (: [])

groupLineWith :: Text -> (Text, [Text])
groupLineWith s = (node, children)
  where
    splitResult = T.split (== ':') s
    node = splitResult !! 0
    children = T.split (== ',') $ splitResult !! 1

parseInput :: ([Text] -> [(Text, [Text])]) -> Text -> Mapping
parseInput groupFunc = foldr f M.empty . groupFunc . T.lines
  where
    f (node, children) acc = updateMapping node acc children

pathsFromNode :: Text -> Mapping -> [[Text]]
pathsFromNode current mapping
    | current `M.notMember` mapping = [["@"] | current == "@"]
    | otherwise = do
        rest <- map (`pathsFromNode` mapping) (fromJust $ M.lookup current mapping)
        map ([current] <>) rest

pathsWithLengths :: [[Text]] -> [([Text], Int)]
pathsWithLengths = map ((,) <*> length)

uniquePath :: NonEmpty ([Text], Int) -> [Text]
uniquePath (p :| []) = fst p
uniquePath (p :| ps) = fst . (!! 0) . fromJust $ find ((== 1) . length) groups
  where
    groups = groupBy (\a b -> snd a == snd b) $ sortBy (compare `on` snd) (p : ps)

part1 :: Text -> Text
part1 =
    T.concat
        . uniquePath
        . NE.fromList
        . pathsWithLengths
        . pathsFromNode "RR"
        . parseInput (map groupLineWith)

trimBranch :: Text -> Text
trimBranch = T.take 1

part2 :: Text -> Text
part2 =
    T.concat
        . map trimBranch
        . uniquePath
        . NE.fromList
        . pathsWithLengths
        . pathsFromNode "RR"
        . parseInput (map groupLineWith)

isBugOrAnt :: Text -> Bool
isBugOrAnt = liftA2 (||) (== "ANT") (== "BUG")

groupLineWithV2 :: Text -> Maybe (Text, [Text])
groupLineWithV2 s = if isBugOrAnt node then Nothing else Just (node, children)
  where
    splitResult = T.split (== ':') s
    node = splitResult !! 0
    children = filter (not . isBugOrAnt) . T.split (== ',') $ splitResult !! 1

part3 :: Text -> Text
part3 =
    T.concat
        . map trimBranch
        . uniquePath
        . NE.fromList
        . pathsWithLengths
        . pathsFromNode "RR"
        . parseInput (mapMaybe groupLineWithV2)
