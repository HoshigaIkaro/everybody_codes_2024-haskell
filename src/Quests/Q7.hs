{-# OPTIONS_GHC -Wno-x-partial #-}

module Quests.Q7
where

-- (run, part1, part2, part3)

import Control.Arrow (second)
import Control.Monad (void)
import Control.Monad.State
import Control.Parallel
import Control.Parallel.Strategies
import Data.Function (on)
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T

run :: IO ()
run = do
    let prefix = "./input/q7"
        firstInputName = prefix <> "/p1.txt"
        secondInputName = prefix <> "/p2.txt"
        thirdInputName = prefix <> "/p3.txt"
        trim = (T.unpack . T.strip . T.pack)
    firstInput <- trim <$> readFile firstInputName
    secondInput <- trim <$> readFile secondInputName
    thirdInput <- trim <$> readFile thirdInputName
    print $ part1 firstInput
    v2 <- part2 secondInput
    print v2
    v3 <- part3 thirdInput
    print v3

updatePower :: Char -> Int -> Int
updatePower '+' = succ
updatePower '-' = max 0 . pred
updatePower _ = id

extractGroups :: String -> (String, String)
extractGroups s = (label, segment)
  where
    splitted = T.split (== ':') $ T.pack s
    label = T.unpack $ splitted !! 0
    segment = T.unpack $ T.filter (/= ',') $ splitted !! 1

extendSegmentsTo :: Int -> String -> String
extendSegmentsTo n = take n . cycle

processSegment :: String -> Int
processSegment = snd . foldr f (10, 0) . reverse
  where
    f char (current, total) =
        let newValue = updatePower char current
         in (newValue, total + newValue)

part1 :: String -> String
part1 = concatMap (fst) . reverse . sortBy (compare `on` (processSegment . snd)) . map (second (extendSegmentsTo 10) . extractGroups) . lines

-- part1 = map (second processSegment) . map (second (extendSegmentsTo 10) . extractGroups) . lines

extractTrack :: String -> String
extractTrack s = (drop 1 topTrack) <> rightMidTrack <> bottomTrack <> leftMidTrack <> "S"
  where
    ls = lines s
    middleLines = init $ drop 1 ls
    topTrack = head ls
    rightMidTrack = map last middleLines
    bottomTrack = reverse $ last ls
    leftMidTrack = reverse $ map head middleLines

knightUpdatePower :: Char -> Char -> Int -> Int
knightUpdatePower '+' = const succ
knightUpdatePower '-' = const pred
knightUpdatePower _ = updatePower

processSegmentV2 :: [(Char, Char)] -> Int
processSegmentV2 pairs = evalState (nextStep $ makeFuncs pairs) (10, 0)

zipToTrack :: String -> String -> [(Char, Char)]
zipToTrack track segment = zip track (cycle segment)

part2 :: String -> IO String
part2 s = do
    track <- extractTrack <$> readFile "./input/q7/p2a.txt"
    let extendedTrack = concat $ replicate 10 track

    return
        $ concatMap
            (fst)
            . reverse
            . sortBy (compare `on` (processSegmentV2 . snd))
            . map (second (zipToTrack extendedTrack) . extractGroups)
        $ lines s
type Point = (Int, Int)

makeBoard :: String -> Map Point Char
makeBoard = M.fromList . filter (flip elem "+-=S" . snd) . concatMap f . flip zip [0 ..] . lines
  where
    f (row, rowNum) =
        zipWith
            (curry (\(col, colNum) -> ((rowNum, colNum), col)))
            row
            [0 ..]

adjacentPoints :: Point -> [Point]
adjacentPoints (row, col) =
    [ (row - 1, col)
    , (row + 1, col)
    , (row, col - 1)
    , (row, col + 1)
    ]

nextPoint :: Point -> Point -> Map Point Char -> Point
nextPoint previousPoint currentPoint mapping =
    head $
        filter (liftA2 (&&) (previousPoint /=) (`M.member` mapping)) $
            adjacentPoints currentPoint

extractTrackGeneral :: Map Point Char -> String
extractTrackGeneral board = board M.! (0, 1) : go (0, 0) (0, 1)
  where
    go previous current
        | board M.! newPoint == 'S' = "S"
        | otherwise = newValue : go current newPoint
      where
        newPoint = nextPoint previous current board
        newValue = board M.! newPoint

allPermutations :: [String]
allPermutations = permutations "+++++---==="

mergeTrackWithSegment :: [(Char, Char)] -> String
mergeTrackWithSegment = foldr f ""
  where
    f ('+', _) acc = '+' : acc
    f ('_', _) acc = '_' : acc
    f (_, char) acc = char : acc

findWinningSegments :: Int -> String -> Int -> [Int]
findWinningSegments numLoops track rivalScore =
    map (f . processSegment) $ S.toList $ S.fromList $ parMap rpar (mergeTrackWithSegment . zipped) allPermutations
  where
    totalTrack = concat $ replicate numLoops track
    zipped segment = zipToTrack totalTrack segment
    -- findWinningSegments numLoops track rivalScore = parMap rpar (f . simulateSegmentForNumLoops numLoops track) allPermutations
    f score = if score > rivalScore then 1 else 0

part3 :: String -> IO Int
part3 s = do
    track <- (extractTrackGeneral . makeBoard) <$> readFile "./input/q7/p3a.txt"
    return $ go track
  where
    go track =
        let numLoops = 11
            (_, rivalSegment) = head . map extractGroups $ lines s
            rivalScore = processSegmentV2 (zipToTrack track rivalSegment)
         in pfold $ findWinningSegments numLoops track rivalScore

nextStep :: [(Int -> Int)] -> State (Int, Int) Int
nextStep [] = return 0
nextStep (f : fs) = do
    (current, total) <- get
    let newVal = f current
        newTotal = total + newVal
    put (newVal, newTotal)
    rest <- nextStep fs
    return $ newVal + rest

makeFuncs :: [(Char, Char)] -> [(Int -> Int)]
makeFuncs = map (uncurry knightUpdatePower)

pfold :: [Int] -> Int
pfold [value] = value
pfold xs = (ys `par` zs) `pseq` (ys + zs)
  where
    len = length xs
    (ys', zs') = splitAt (len `div` 2) xs
    ys = pfold ys'
    zs = pfold zs'