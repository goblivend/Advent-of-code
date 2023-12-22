module Main where

import Data.Char
import Data.Maybe ( fromJust, isJust )
import Debug.Trace(trace)
import Data.Tuple
import Data.Tuple.Extra
import Data.List.Split
import Data.List.Unique
import Data.Set (Set, member, fromList, toList)
import Data.Map (Map, (!))
-- import Data.NumInstances.Tuple
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

data Brick = Brick { one :: (Int, Int, Int), two:: (Int, Int, Int)} deriving (Eq, Ord, Show)
type Input = [Brick]

cmp (Brick (x1, y1, z1) (x2, y2, z2)) (Brick (x1', y1', z1') (x2', y2', z2')) = compare (z1, z2, y1, y2, x1, x2) (z1', z2', y1', y2', x1', x2')

parseInput :: String -> Input
parseInput = L.sortBy cmp . map parseBrick . lines
    where
        parseBrick s = Brick (cosOf oneEnd) (cosOf secondEnd)
            where
                [oneEnd, secondEnd] = splitOn "~" s
                cosOf c =  (read ("(" ++ c ++ ")"))

restsOn :: Input -> Brick -> Input
restsOn [] _ = []
restsOn ((e@(Brick xyz1' xyz2')):l) (b@(Brick xyz1 xyz2))
    | thd3 xyz2' /= thd3 xyz1 - 1 = restsOn l b -- Not even at the level below
    | any (`elem` cubes) cubes' = e: restsOn l b -- Some are just below it
    | otherwise = restsOn l b
        where
            cubes =  [(x, y) | x<-[fst3 xyz1 ..fst3 xyz2 ], y<-[snd3 xyz1 ..snd3 xyz2 ]]
            cubes' = [(x, y) | x<-[fst3 xyz1'..fst3 xyz2'], y<-[snd3 xyz1'..snd3 xyz2']]

fall :: Input -> Input -> Input
fall [] acc = acc
fall ((b@(Brick (x1, y1, z1) (x2, y2, z2))):l) acc
    | z1 == 1                = fall l (b:acc)
    | not . null $ restingOn = fall l (b:acc)
    | otherwise              = fall  ((Brick (x1, y1, z1-1) (x2, y2, z2-1)):l) acc
        where restingOn = restsOn acc b

supports :: Input -> Brick -> Input
supports [] _ = []
supports ((e@(Brick xyz1' xyz2')):l) (b@(Brick xyz1 xyz2))
    | thd3 xyz1' /= thd3 xyz2 + 1 = supports l b -- Not even at the level below
    | any (`elem` cubes) cubes' = e: supports l b -- Some are just below it
    | otherwise = supports l b
        where
            cubes =  [(x, y) | x<-[fst3 xyz1 ..fst3 xyz2 ], y<-[snd3 xyz1 ..snd3 xyz2 ]]
            cubes' = [(x, y) | x<-[fst3 xyz1'..fst3 xyz2'], y<-[snd3 xyz1'..snd3 xyz2']]

part1 :: Input -> Int
part1 inp = (S.size . S.fromList $ restsOnMany ++ supportsNone)
    where
        restsOnMany = filter (\b -> all ((> 1) . length . restsOn rested) . supports rested $ b) .
         concat .filter ((> 1) .length)$map(restsOn rested) rested
        supportsNone = filter ((== 0) .length . supports rested) $ rested
        rested = L.sortBy cmp $ fall inp []


chain :: Input -> Set Brick -> Int -> Set Brick
chain inp b i
    | (fallIfNonPresent `S.difference` b) == S.empty = b
    | otherwise = {-trace (show ((length inp, S.size b, S.size fallIfNonPresent), fallIfNonPresent))-} more
        where
            more = chain (inp) (b `S.union`fallIfNonPresent) (i+1)
            fallIfNonPresent = S.fromList $ filter (all (`elem` b) . restsOn inp) . sortUniq . concat $ map (supports inp) $ S.toList b


part2 :: Input -> Int
part2 inp = sum  {- .map snd . map (\e -> trace (show ((one $ fst e, two $ fst e)) ++ ": " ++ show (snd e)++ ",") e ) . zip rested -} $ map (\b ->(+ (-1)) . S.size $ chain rested (S.singleton b) 0) rested
    where
        rested = fall inp []

main :: IO ()
main = do
    content <- readFile "minput.txt"
    let inp = parseInput content
    --
    -- print $ part1 inp
    -- putStr $ if -50 < part2 inp then "" else ""
    print $ part2 inp
    -- putStr "\n\n\n\n\n"
    -- print $ chain ninp  [Brick {one = (0,0,2), two = (2,0,2)},Brick {one = (0,2,2), two = (2,2,2)}] 0
    let rested = fall inp []
    -- print $ map (\b -> (one b, two b)) $ rested
    print $  chain rested (S.singleton (Brick (3, 2, 1) (3, 4, 1))) 0
