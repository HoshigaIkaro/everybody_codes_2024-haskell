{-# OPTIONS_GHC -Wno-x-partial #-}

module Quests.Q12 (run, part1, part2, part3) where

import Control.Applicative (asum)
import Control.Arrow (Arrow (second), first, (***))
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (parMap, rdeepseq, rpar)
import Data.Function (on)
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (isNothing, mapMaybe)
import Data.Ord (Down (Down), comparing)
import Data.Text qualified as T

run :: IO ()
run = mempty

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
findMaxBound = ((\a -> a - div a 3)) . uncurry max . foldr (\(a, b) (c, d) -> (max a c, max b d)) (0, 0)
  where
    -- findMaxBound = succ . flip div 2 . maximum . map fst

    -- f (a, b) (c, d) = (max a c, max b d)
    g (a, b) = (a - div a 4, b - div b 4)

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

keepLowerScore :: Int -> Maybe Int -> Maybe Int
keepLowerScore newScore mScore = Just $ maybe newScore (max newScore) mScore

unionKeepLower :: PointTimeToScore -> PointTimeToScore -> PointTimeToScore
unionKeepLower = M.unionWith min

precomputeMappingStartingAtTime :: Int -> Int -> PointTimeToScore
precomputeMappingStartingAtTime bound startTime = a `par` b `par` c `pseq` ((a `unionKeepLower` b) `unionKeepLower` c)
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
findMinMatch mapping = minimum . mapMaybe (flip M.lookup mapping) . zipWithTimeStep 0 . meteorPoints

part3 :: String -> Int
part3 s = sum $ parMap rpar (findMatch precomputed 0) startPoints
  where
    f [a, b] = Just (a, b)
    f _ = Nothing
    bound = findMaxBound startPoints
    precomputed = precomputeMappingAll bound
    startPoints = mapMaybe (f . map read . words) $ lines s

timeToReach :: Point -> Point -> Int -> Int
timeToReach start target = length . takeWhile (/= target) . trajectoryPoints start

powerToReachFrom :: Point -> Point -> Int -> Maybe Int
powerToReachFrom start target@(tx, ty) time = case dropWhile ((== False) . snd) maybeReachableList of
    [] -> Nothing
    list -> Just $ fst $ head list
  where
    powers = [1 .. max tx ty]
    distanceWorks = (<= time) . timeToReach start target
    success = liftA2 (&&) distanceWorks (canReachFrom start target)
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
    go time point = case scoreToReach point time of
        Just score -> score
        Nothing -> go (succ time) (nextMeteorPoint point)

slope :: Point -> Point -> Double
slope (x, y) (tx, ty) = fromIntegral (ty - y) / fromIntegral (tx - x)