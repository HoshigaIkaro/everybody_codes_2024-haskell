module Quests.Q17 (run, part1, part2) where

import Control.Arrow
import Control.Lens (Field3 (_3), (^.))
import Data.Function
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T

run :: IO ()
run = do
    let prefix = "./input/q17"
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

dist :: Point -> Point -> Int
dist (a, b) (c, d) = abs (a - c) + abs (d - b)

pStars :: String -> [Point]
pStars s = stars
  where
    ls = lines s
    processRow rowNum = filter ((/= '.') . snd) . zipWith (curry (first (rowNum,))) [0 ..]
    stars = concatMap (map fst . uncurry processRow) $ zip [0 ..] ls

minDistToStarInConst :: Point -> Map Point [Point] -> (Point, Point, Int)
minDistToStarInConst point = minimumBy (compare `on` (^. _3)) . map ((,,) <$> (const point) <*> id <*> dist point) . M.keys

mAddStar :: (Map Point [Point], Int) -> [Point] -> (Map Point [Point], Int)
mAddStar res [] = res
mAddStar (mapping, currentDist) rest = mAddStar (newMapping, currentDist + extraDist) newRest
  where
    (point, closest, extraDist) = minimumBy (compare `on` (^. _3)) $ map (flip minDistToStarInConst mapping) rest
    f a = Just . maybe [a] (a :)
    updated k v = M.alter (f v) k
    newMapping = updated point closest (updated closest point mapping)
    newRest = rest \\ [point]

makeConstellationAndDist :: [Point] -> (Map Point [Point], Int)
makeConstellationAndDist = (mAddStar <$> ((,0) . (M.fromList . map (,[]) . take 1)) <*> (drop 1))

constellationSize :: (Map Point [Point], Int) -> Int
constellationSize = ((+) <$> (M.size . fst) <*> snd)

part1 :: String -> Int
part1 = constellationSize . makeConstellationAndDist . pStars

part2 :: String -> Int
part2 = constellationSize . makeConstellationAndDist . pStars

mFindPoint :: [Point] -> [Point] -> Maybe Point
mFindPoint cluster remaining = find (\p -> any ($ p) distFuncs) remaining
  where
    distFuncs = map (((< 6) .) . dist) cluster

makeCluster :: [Point] -> Maybe ([Point], [Point])
makeCluster [] = Nothing
makeCluster (x : rest) = Just $ go [x] rest
  where
    go current remaining = case mFindPoint current remaining of
        Nothing -> (current, remaining)
        Just point -> go (point : current) (delete point remaining)

clusterStars :: [Point] -> [[Point]]
clusterStars remaining = case makeCluster remaining of
    Nothing -> []
    Just (cluster, newRemaining) -> cluster : clusterStars newRemaining

-- go current (x:xs)

-- part3 :: String -> Int
part3 = product . take 3 . sortOn negate . map (constellationSize . makeConstellationAndDist) . clusterStars . pStars