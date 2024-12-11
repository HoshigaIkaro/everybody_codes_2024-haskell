{-# LANGUAGE OverloadedStrings #-}

module Quests.Q16 (run, part1, part2) where

import Control.Arrow
import Control.Monad (void)
import Data.Either (fromRight)
import Data.Function
import Data.Heap (Heap)
import Data.Heap qualified as H
import Data.Ix (Ix (range))
import Data.List
import Data.Map qualified as M
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

run :: IO ()
run = do
    let prefix = "./input/q16"
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

stripEnds :: Text -> Text
stripEnds = T.unlines . map T.stripEnd . T.lines

pFace :: Parser Text
pFace = takeP (Just "Face") 3

pWheelLine :: Parser [Text]
pWheelLine = sepBy pFace (char ' ')

pWheelLines :: Parser [[Text]]
pWheelLines = sepBy pWheelLine eol

pWheelRotations :: Parser [Int]
pWheelRotations = sepBy L.decimal (char ',')

pInputParser :: Parser [(Int, Map Int Text)]
pInputParser = do
    wheelRotations <- pWheelRotations
    void $ eol
    void $ eol
    wheelLines <- pWheelLines
    let toWheelMap = M.fromList . zip [0 ..] . takeWhile (/= "   ")
    pure (zipWith (\r m -> (mod r (M.size m), m)) wheelRotations $ map toWheelMap (transpose wheelLines))

pInput :: String -> [(Int, Map Int Text)]
pInput = fromRight [] . runParser pInputParser "" . stripEnds . T.pack

getFacesAtPull :: Int -> [(Int, Map Int Text)] -> [Text]
getFacesAtPull n = map f
  where
    f (rotationsPerPull, mapping) = mapping M.! ((n * rotationsPerPull) `mod` (M.size mapping))

part1 :: String -> Text
part1 = T.unwords . getFacesAtPull 100 . pInput

frequenciesFromFaces :: [Text] -> Map Char Int
frequenciesFromFaces = T.foldr f M.empty . T.concat
  where
    f = M.alter (Just . maybe 1 succ)

byteCoinsWonWithFaces :: [Text] -> Int
byteCoinsWonWithFaces = sum . map (subtract 2) . M.elems . M.filter (>= 3) . frequenciesFromFaces

byteCoinsWonAt :: [(Int, Map Int Text)] -> Int -> Int
byteCoinsWonAt m n = byteCoinsWonWithFaces $ getFacesAtPull n m

toEyeWheels :: Map Int Text -> Map Int Text
toEyeWheels = M.map ((<>) <$> (T.take 1) <*> (T.drop 2))

part2 :: String -> Int
part2 s = coinsWonInCycle * numCycles + coinsWonAfterCycle
  where
    wheels = pInput s
    eyeWheels = map (second toEyeWheels) wheels
    targetPulls = 202420242024
    cycleLen :: Int
    cycleLen = foldr lcm 1 $ map ((M.size . snd)) wheels
    mapping = M.fromList $ map ((,) <$> id <*> byteCoinsWonAt eyeWheels) [1 .. cycleLen]
    coinsWonInCycle = sum $ M.elems mapping
    numCycles = targetPulls `div` cycleLen
    pullsAfterCycle = targetPulls `mod` cycleLen
    coinsWonAfterCycle = sum $ map (mapping M.!) [1 .. pullsAfterCycle]

getFacesAtPullV2 :: Int -> Int -> [(Int, Map Int Text)] -> [Text]
getFacesAtPullV2 lef n = map f
  where
    f (rotationsPerPull, mapping) = mapping M.! ((n * rotationsPerPull + lef) `mod` (M.size mapping))

byteCoinsWonAtV2 :: [(Int, Map Int Text)] -> Int -> Int -> Int
byteCoinsWonAtV2 m lef n = sum . map (subtract 2) . M.elems . M.filter (>= 3) . frequenciesFromFaces $ getFacesAtPullV2 lef n m

findMinPulls :: [(Int, Map Int Text)] -> Int
findMinPulls wheels = minimum $ go initialHeap
  where
    initialHeap = H.singleton (H.Entry 0 (0, 0))
    eyeWheels = map (second toEyeWheels) wheels
    go heap
        | H.null heap = []
        | 256 == snd (H.payload (H.minimum heap)) = map H.priority $ H.toUnsortedList heap
        | otherwise = go newHeap
      where
        minStates = H.toUnsortedList heap
        newPulls (l, r) = [(l - 1, r + 1), (l, r + 1), (l + 1, r + 1)]
        newCoins :: Int -> (Int, Int) -> Int
        newCoins coins = (coins +) . uncurry (byteCoinsWonAtV2 eyeWheels)
        newPullsForState (H.Entry coins pull) = map (H.Entry <$> (newCoins coins) <*> id) $ newPulls pull
        newHeap = H.take 1000 $ H.fromList $ nubBy (\a b -> H.payload a == H.payload b) $ concatMap newPullsForState minStates

findMaxPulls :: [(Int, Map Int Text)] -> Int
findMaxPulls wheels = maximum $ go initialHeap 0
  where
    initialHeap = H.singleton (H.Entry 0 0)
    eyeWheels = map (second toEyeWheels) wheels
    go heap r
        | H.null heap = []
        | 256 == r = map (negate . H.priority) $ H.toUnsortedList heap
        | otherwise = go newHeap (r + 1)
      where
        minStates = H.toUnsortedList heap
        newPulls l = [l - 1, l, l + 1]
        newCoins coins = (+ coins) . negate . flip (byteCoinsWonAtV2 eyeWheels) (r + 1)
        newPullsForState (H.Entry coins pull) = map (H.Entry <$> (newCoins coins) <*> id) $ newPulls pull
        newHeap = H.take 200 $ H.fromList $ nubBy (\a b -> H.payload a == H.payload b) $ concatMap newPullsForState minStates

findMinMaxPulls :: [(Int, Map Int Text)] -> [Int]
findMinMaxPulls wheels = go initialHeap initialHeap 0
  where
    initialHeap = H.singleton (H.Entry 0 0)
    eyeWeheels = map (second toEyeWheels) wheels
    go maxHeap minHeap r
        | H.null maxHeap || H.null minHeap = []
        | r == 256 = [maxCoins, minCoins]
        | otherwise = go newMaxHeap newMinHeap newR
      where
        newR = r + 1
        newPulls lef = [lef - 1, lef, lef + 1]
        minStates = H.toUnsortedList minHeap
        maxStates = H.toUnsortedList maxHeap
        newCoins f coins = (+ coins) . f . flip (byteCoinsWonAtV2 eyeWeheels) newR
        newPullsForState f (H.Entry coins pull) = map (H.Entry <$> (newCoins f coins) <*> id) $ newPulls pull
        newPullsForMinState = newPullsForState id
        newPullsForMaxState = newPullsForState negate
        newHeap f = H.take 100 . H.fromList . nubBy (\a b -> H.payload a == H.payload b) . concatMap f
        newMinHeap = newHeap newPullsForMinState minStates
        newMaxHeap = newHeap newPullsForMaxState maxStates
        minCoins = H.priority $ H.minimum minHeap
        maxCoins = negate . H.priority $ H.minimum maxHeap

part3 :: String -> String
part3 = unwords . map show . findMinMaxPulls . pInput
