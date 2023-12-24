module Main where

import Data.Char
import Data.Maybe ( Maybe, fromJust, isJust )
import Debug.Trace(trace)
import Data.Tuple
import Data.Tuple.Extra;
import Control.Parallel.Strategies
import Data.List
import Data.List.Utils
import Data.Set (Set, member, fromList, toList)
import Data.Map (Map, (!))
import Data.NumInstances.Tuple
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

type Input = [((Double, Double, Double), (Double, Double, Double))]

parseInput :: String -> Input
parseInput = map readLine . lines
    where
        readLine line = (p, v)
            where
                (ps, '@':vs) = break (=='@') line
                readCo = map read . words . replace "," " " . filter (/= ' ')
                p = (\[x, y, z] -> (x, y, z))$ readCo ps
                v = (\[x, y, z] -> (x, y, z))$ readCo vs

inBounds :: ((Double, Double), (Double, Double)) -> (Double, Double) -> Bool
inBounds ((minX, maxX), (minY, maxY)) (x, y) = minX <= x && x <= maxX && minY <= y && y <= maxY

doIntersect2DNoTime :: ((Double, Double), (Double, Double)) -> ((Double, Double), (Double, Double)) -> ((Double, Double), (Double, Double)) -> Bool
doIntersect2DNoTime bounds (p1, v1) (p2, v2)
    | t1 > 0 && t2 > 0 = inBounds bounds p
    | otherwise        = False
        where
            (t1, t2) = algo (p1, v1) (p2, v2)

            p = p1 + (fst v1 * t1, snd v1 * t1)
            p' = p2 + (fst v2 * t2, snd v2 * t2)

algo :: ((Double, Double), (Double, Double)) -> ((Double, Double), (Double, Double)) -> (Double, Double)
algo ((px, py), (vx, vy)) ((px', py'), (vx', vy')) = (t, t')
    where
        (dpx, dpy) = (px'-px, py'-py)
        t = (dpx + vx'*t') / vx
        t' = (dpy - (vy/vx)*dpx) / (vy*vx'/vx - vy')

to2D :: ((Double, Double, Double), (Double, Double, Double)) ->((Double, Double), (Double, Double))
to2D ((px, py, _), (vx, vy, _)) = ((px, py), (vx, vy))

to1D :: ((Double, Double, Double), (Double, Double, Double)) ->(Double, Double)
to1D ((px, _, _), (vx, _, _)) = (px, vx)

part1 :: Input -> Int
part1 = (`div` 2) . length . filter (uncurry (doIntersect2DNoTime ((200000000000000, 400000000000000), (200000000000000, 400000000000000)))) . (\l -> [(h1, h2) | h1<-l, h2<-l, h1 /= h2]) . map to2D

-- lstEq :: [Double] -> Bool
-- lstEq [] = True
-- lstEq (e:l) = all (==e) l

-- doIntersect3D ::  ((Double, Double, Double), (Double, Double, Double)) -> ((Double, Double, Double), (Double, Double, Double)) -> Bool
-- doIntersect3D (p1, v1) (p2, v2) = all (>=0) ts && lstEq ts
--     where
--         ts = filter (not . isNaN) [tx, ty, tz]
--         (tx, ty, tz) = (p1-p2) / (v2-v1)

-- doIntersect2D ::  ((Double, Double), (Double, Double)) -> ((Double, Double), (Double, Double)) -> Bool
-- doIntersect2D (p1, v1) (p2, v2) = all (>=0) ts && lstEq ts
--     where
--         ts = filter (not . isNaN) [tx, ty]
--         (tx, ty) = (p1-p2) / (v2-v1)

-- doIntersect1D :: (Double, Double) -> (Double, Double) -> Bool
-- doIntersect1D (p1, v1) (p2, v2) = t >= 0
--     where
--         t = (p1-p2) / (v2-v1)

-- part2 :: Input -> Double
-- part2 inp = head . map ((\(x, y, z) -> (x + y + z)) . fst . fst) $ possibleXYZs
--     where
--         -- allPossibles :: [(Double, Double)]
--         allPossiblesX = [(p, v) | v<-[-11], p<-[291669802654110]] -- [((24, 13, 10), (-3, 1, 2))] --
--         allPossiblesY = [(p, v) | v<-[330], p<-[103597826800230]] -- [((24, 13, 10), (-3, 1, 2))] --
--         allPossiblesZ = [(p, v) | v<-[91 ], p<-[251542427650413]] -- [((24, 13, 10), (-3, 1, 2))] --

--         allWithX :: [((Double, Double), [((Double, Double, Double), (Double, Double, Double))])]
--         allWithX = zip allPossiblesX . repeat $ inp

--         possibleXs :: [((Double, Double), [((Double, Double, Double), (Double, Double, Double))])]
--         possibleXs = filter (\(x, l) -> all (doIntersect1D x) $ map to1D l) allWithX

--         allWithXY :: [(((Double, Double), (Double, Double)), [((Double, Double, Double), (Double, Double, Double))])]
--         allWithXY = concat $ map (\((px, vx), l) -> map (\(py, vy) -> (((px, py), (vx, vy)), l)) allPossiblesY) possibleXs

--         possibleXYs :: [(((Double, Double), (Double, Double)), [((Double, Double, Double), (Double, Double, Double))])]
--         possibleXYs = filter (\(x, l) -> all (doIntersect2D x) $ map to2D l) allWithXY

--         allWithXYZ :: [(((Double, Double, Double), (Double, Double, Double)), [((Double, Double, Double), (Double, Double, Double))])]
--         allWithXYZ = concat $ map (\(((px, py), (vx, vy)), l) -> map (\(pz, vz) -> (((px, py, pz), (vx, vy, vz)), l)) allPossiblesZ) possibleXYs

--         possibleXYZs :: [(((Double, Double, Double), (Double, Double, Double)), [((Double, Double, Double), (Double, Double, Double))])]
--         possibleXYZs = filter (\(x, l) -> all (doIntersect3D x) l) allWithXYZ


myIntersec :: Set Int -> Set Int -> Set Int
myIntersec s1 s2
    | null s1 = s2
    | null s2 = s1
    | otherwise = S.intersection s1 s2

combineOne :: Set Int -> (Double, Double) -> (Double, Double) -> Set Int
combineOne s (p1, v1) (p2, v2)
    | v1 /= v2  = s
    | otherwise = myIntersec s vs
        where
            dp = round $ p2-p1
            -- if on an axis, both particles have the same speed
            -- the distance between the two particle must be a multiple of the relative speed
            vs = S.fromList [v | v<-[-5000..5000], v /= round v1, dp `mod` (v-round v1) == 0]

combine :: (Set Int, Set Int, Set Int) -> (((Double, Double, Double), (Double, Double, Double)), ((Double, Double, Double), (Double, Double, Double))) -> (Set Int, Set Int, Set Int)
combine (setX, setY, setZ) ((p1, v1), (p2, v2)) = (combineOne setX (fst3 p1, fst3 v1) (fst3 p2, fst3 v2),
                                                   combineOne setY (snd3 p1, snd3 v1) (snd3 p2, snd3 v2),
                                                   combineOne setZ (thd3 p1, thd3 v1) (thd3 p2, thd3 v2))

part2 :: Input -> Int
part2 inp = solveFor v $ head particleCombinations
    where
        particleCombinations = [(h1, h2) | h1<-inp, h2<-inp, h1 /= h2]
        (svx, svy, svz) = foldl (\s c -> combine s c) (S.empty, S.empty, S.empty) particleCombinations
        v = (head $ S.toList svx, head $ S.toList svy, head $ S.toList svz)
        solveFor :: (Int, Int, Int) -> (((Double, Double, Double), (Double, Double, Double)), ((Double, Double, Double), (Double, Double, Double))) -> Int
        solveFor (vx, vy, vz) (((px1, py1, pz1), (vx1, vy1, vz1)), ((px2, py2, pz2), (vx2, vy2, vz2))) = x + y + z
            where
                (vdx, vdy, vdz) = (fromIntegral vx, fromIntegral vy, fromIntegral vz)
                mA = (vy1-vdy) / (vx1-vdx)
                mB = (vy2-vdy) / (vx2-vdx)
                cA = py1 - (mA*px1)
                cB = py2 - (mB*px2)
                x = round $ (cB-cA)/(mA-mB)
                y = round $ mA * fromIntegral x + cA
                t = round $ (fromIntegral x - px1)/(vx1 - vdx)
                z = round pz1 + (round vz1 - vz)*t

main :: IO ()
main = do
    content <- readFile "input.txt"
    let inp = parseInput content
    -- print inp
    print $ part1 inp
    print $ part2 inp
