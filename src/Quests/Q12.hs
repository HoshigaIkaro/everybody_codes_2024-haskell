{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wunused-top-binds #-}

module Quests.Q12 (run, part1, part2, part3) where

import Control.Applicative (asum)
import Control.Arrow (Arrow (second), first, (***))
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (parMap, rpar)
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Ord (Down (Down), comparing)
import qualified Data.Text as T

run :: IO ()
run = do
    let prefix = "./input/q12"
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

canReachFrom :: Point -> Point -> Int -> Bool
canReachFrom (x, y) (tx, ty) power = reachableInCruise || reachableInDescent
  where
    finalSlope :: Double
    finalSlope = fromIntegral (ty - (y + power)) / fromIntegral (tx - (x + 2 * power))
    reachableInCruise = tx `elem` [x + power .. x + 2 * power] && ty == y + power
    reachableInDescent = finalSlope == -1.0
maxPowerBoundFor :: Point -> Point -> Int
maxPowerBoundFor (x, y) (tx, _) = (tx + y - x) `div` 2

canUseCatapultPower :: Point -> Point -> Maybe Int
canUseCatapultPower start target = find (canReachFrom start target) [1 .. maxPowerBoundFor start target]

parseMap :: String -> Map Point Char
parseMap s = M.fromList $ concatMap (filter ((/= '.') . snd)) (zipWith zip allPositions ls)
  where
    ls = map (drop 1) . drop 1 . reverse $ lines s
    height = length ls
    width = length $ head ls
    allPositions = map (\row -> map (,row) [0 .. width - 1]) [0 .. height - 1]

type CatapultLocations = [Point]
type TargetPairs = [(Point, Char)]

extractPair :: Map Point Char -> (CatapultLocations, TargetPairs)
extractPair = (M.keys *** M.toList) . M.partition (`notElem` "TH")

type Score = Int

timesNeeded :: Char -> Int
timesNeeded 'T' = 1
timesNeeded 'H' = 2
timesNeeded _ = 0

tryDestroy :: CatapultLocations -> (Point, Char) -> Maybe Score
tryDestroy [] _ = Nothing
tryDestroy (c@(_, y) : cs) target@(targetPoint, char) = case canUseCatapultPower c targetPoint of
    Just power -> Just $ (y + 1) * timesNeeded char * power
    Nothing -> tryDestroy cs target

part1 :: String -> Int
part1 s = sum $ mapMaybe (tryDestroy processedCatapults) processedTargets
  where
    (catapults, targets) = extractPair $ parseMap s
    processedCatapults = sortBy (comparing Down) catapults
    processedTargets = sortBy (comparing (second Down)) targets

part2 :: String -> Int
part2 s = sum $ mapMaybe (tryDestroy processedCatapults) processedTargets
  where
    (catapults, targets) = extractPair $ parseMap s
    processedCatapults = sortBy (comparing Down) catapults
    processedTargets = sortBy (comparing (second Down)) targets

findMaxBound :: [Point] -> Int
findMaxBound = (\a -> a - div a 3) . uncurry max . foldr (\(a, b) (c, d) -> (max a c, max b d)) (0, 0)

trajectoryPoints :: Point -> Int -> [Point]
trajectoryPoints (x, y) power = ascendingPoints <> cruisePoints <> descendingPoints
  where
    ascendingPoints = take (succ power) $ iterate (succ *** succ) (x, y)
    cruisePoints = take power $ iterate (first succ) (first succ $ last ascendingPoints)
    descendingPoints = drop 1 . takeWhile ((>= 0) . snd) $ iterate (succ *** pred) (last cruisePoints)

zipWithTimeStep :: Int -> [Point] -> [(Point, Int)]
zipWithTimeStep startTime = flip zip [startTime ..]

type PointTimeToScore = Map (Point, Int) Int

precomputeFor :: Int -> Point -> Int -> Int -> PointTimeToScore -> PointTimeToScore
precomputeFor startTime point multiplyer bound mapping = foldr (uncurry M.insert) mapping $ concatMap f powers
  where
    powers = [1 .. bound]
    computeForPower power = map (,multiplyer * power) . zipWithTimeStep startTime $ trajectoryPoints point power
    -- f = map (uncurry M.singleton) . computeForPower
    f = computeForPower

keepMin :: Int -> Maybe Int -> Maybe Int
keepMin newScore mScore = Just $ maybe newScore (min newScore) mScore

unionKeepMin :: PointTimeToScore -> PointTimeToScore -> PointTimeToScore
unionKeepMin = M.unionWith min

precomputeMappingStartingAtTime :: Int -> Int -> PointTimeToScore
precomputeMappingStartingAtTime bound startTime = a `par` b `par` c `pseq` ((a `unionKeepMin` b) `unionKeepMin` c)
  where
    a = precomputeFor startTime (0, 0) 1 bound mempty
    b = precomputeFor startTime (0, 1) 2 bound mempty
    c = precomputeFor startTime (0, 2) 3 bound mempty

precomputeMappingAll :: Int -> PointTimeToScore
precomputeMappingAll bound = precomputeMappingStartingAtTime bound 0

nextMeteorPoint :: Point -> Point
nextMeteorPoint = pred *** pred

meteorPoints :: Point -> [Point]
meteorPoints = takeWhile ((>= 0) . snd) . iterate nextMeteorPoint

findMatch :: PointTimeToScore -> Int -> Point -> Int
findMatch _ _ (_, -1) = 0
findMatch mapping time point@(x, y) =
    case lookupPossibleValues of
        [] -> findMatch mapping (time + 1) (pred x, pred y)
        list -> minimum list
  where
    possibleTimes = [1 .. time]
    possiblePointTimes = map (point,) possibleTimes
    lookupPossibleValues = mapMaybe (`M.lookup` mapping) possiblePointTimes

findMinMatch :: PointTimeToScore -> Point -> Int
findMinMatch mapping = minimum . mapMaybe (`M.lookup` mapping) . zipWithTimeStep 0 . meteorPoints

pStartPoints :: String -> [Point]
pStartPoints = mapMaybe (f . map read . words) . lines
  where
    f [a, b] = Just (a, b)
    f _ = Nothing

part3 :: String -> Int
part3 s = sum $ parMap rpar (findMatch precomputed 0) startPoints
  where
    startPoints = pStartPoints s
    bound = findMaxBound startPoints
    precomputed = precomputeMappingAll bound

-- Failed functions

timeToReach :: Point -> Point -> Int -> Int
timeToReach start target = length . takeWhile (/= target) . trajectoryPoints start

powerToReachFrom :: Point -> Point -> Int -> Maybe Int
powerToReachFrom start target@(tx, ty) time = case dropWhile (not . snd) maybeReachableList of
    [] -> Nothing
    list -> Just $ fst $ head list
  where
    powers = reverse [1 .. max tx ty]
    distanceWorks = (<= time) . timeToReach start target
    success = liftA2 (&&) (canReachFrom start target) distanceWorks
    maybeReachableList = map ((,) <*> success) powers

scoreToReach :: Point -> Int -> Maybe Int
scoreToReach target time = asum [a, b, c]
  where
    f start@(_, y) = (* succ y) <$> powerToReachFrom start target time
    a = f (0, 0)
    b = f (0, 1)
    c = f (0, 2)

solveMeteor :: Point -> Int
solveMeteor = go 0
  where
    go _ (_, -1) = -1
    go time point = case scoreToReach point time of
        Just score -> score
        Nothing -> go (succ time) (nextMeteorPoint point)

slope :: Point -> Point -> Double
slope (x, y) (tx, ty) = fromIntegral (ty - y) / fromIntegral (tx - x)

mDistTo :: Point -> Point -> Maybe Int
mDistTo start@(x, y) target@(tx, ty)
    -- Only discrete time steps possible
    | initialSlope == 1.0 = Just tx
    | otherwise = Nothing
  where
    minPower = 1
    maxPower = ty - y
    initialSlope = slope start target
    finalSlope :: Double
    finalSlope = slope (x + 2 * maxPower, y + maxPower) target
    reachableInCruise = tx `elem` [x + maxPower .. x + 2 * maxPower] && ty == y + maxPower
    reachableInDescent = finalSlope == -1.0

mSolve :: Point -> Point -> Maybe Int
mSolve start@(x, y) target@(tx, ty)
    -- Only discrete time steps possible
    | initialSlope == 1.0 = Just $ succ y * div tx 2
    | otherwise = Nothing
  where
    minPower = 1
    maxPower = ty - y
    initialSlope = slope start target
    finalSlope :: Double
    finalSlope = slope (x + 2 * maxPower, y + maxPower) target
    reachableInCruise = tx `elem` [x + maxPower .. x + 2 * maxPower] && ty == y + maxPower
    reachableInDescent = finalSlope == -1.0
