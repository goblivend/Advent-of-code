module Main where

import Data.Char
import Data.Maybe ( fromJust, isJust )
import Debug.Trace(trace)
import Data.Tuple
import Data.Tuple.Extra
import Data.List.Split
import Data.Set (Set, member, fromList, toList)
import Data.Map (Map, (!))
import qualified Data.Set as S
import qualified Data.Map as M


data Workflow = Accepted | Rejected | MoveTo {dst :: String} | Conditional {cond :: ((Char, Char, Int), Part -> Bool), ifTrue :: Workflow} deriving (Show)
data Part = Part {x::Int, m::Int, a::Int, s::Int} deriving (Show, Read, Eq, Ord)

data Input = Input {workflows :: Map String [Workflow], parts :: [Part]} deriving (Show)

instance Show (Part -> Bool) where
  show f = "<function>"

readCond :: String -> ((Char, Char, Int), Part -> Bool)
readCond ('x':'<':nb) = (('x', '<', read nb), (< (read nb)) . x)
readCond ('x':'>':nb) = (('x', '>', read nb), (> (read nb)) . x)
readCond ('m':'<':nb) = (('m', '<', read nb), (< (read nb)) . m)
readCond ('m':'>':nb) = (('m', '>', read nb), (> (read nb)) . m)
readCond ('a':'<':nb) = (('a', '<', read nb), (< (read nb)) . a)
readCond ('a':'>':nb) = (('a', '>', read nb), (> (read nb)) . a)
readCond ('s':'<':nb) = (('s', '<', read nb), (< (read nb)) . s)
readCond ('s':'>':nb) = (('s', '>', read nb), (> (read nb)) . s)

readFlow :: String -> Workflow
readFlow s
    | ':' `elem` s = let [c, n] = splitOn ":" s
                    in Conditional (readCond c) (readFlow n)
    | s == "A" = Accepted
    | s == "R" = Rejected
    | otherwise = MoveTo s
readWorkflow :: String -> (String, [Workflow])
readWorkflow s = (name, wrkflws)
    where
        name = takeWhile isAlpha s
        wrkflws = map readFlow . splitOn "," $ init $ drop (length name + 1) s

parseInput :: String -> Input
parseInput s = Input workflws prts
    where
        [wrkStr, prtStr] = splitOn "\n\n" s

        workflws = M.fromList . map readWorkflow $ lines wrkStr
        prts = map (read . ("Part" ++)) $ lines prtStr


accept :: Map String [Workflow] -> String -> Part -> Bool
accept wrkfls curr p = acceptFlow (wrkfls ! curr)
    where
        acceptFlow ((Accepted):l) = True
        acceptFlow ((Rejected):l) = False
        acceptFlow ((MoveTo s):l) = accept wrkfls s p
        acceptFlow ((Conditional c r):l)
            | (snd c) p       = acceptFlow [r]
            | otherwise = acceptFlow l

part1 :: Input -> Int
part1 (Input wkfs prts) = sum . map (\p -> (x p) + m p + a p + s p) $  filter (accept wkfs "in") prts

solve :: Map String [Workflow] -> String -> Map Char (Int, Int) -> Int
solve wrkfls curr p = solveW (wrkfls ! curr) p
    where
        splitPart :: (Char, Char, Int) -> Map Char (Int, Int) -> (Map Char (Int, Int), Map Char (Int, Int))
        splitPart ('x', '<', nb) p = (M.insert 'x' (max (fst (p ! 'x')) nb, snd (p ! 'x')) p, M.insert 'x' (fst (p ! 'x'), min (snd (p ! 'x')) (nb - 1)) p)
        splitPart ('x', '>', nb) p = (M.insert 'x' (fst (p ! 'x'), min (snd (p ! 'x')) nb) p, M.insert 'x' (max (fst (p ! 'x')) (nb + 1), snd (p ! 'x')) p)
        splitPart ('m', '<', nb) p = (M.insert 'm' (max (fst (p ! 'm')) nb, snd (p ! 'm')) p, M.insert 'm' (fst (p ! 'm'), min (snd (p ! 'm')) (nb - 1)) p)
        splitPart ('m', '>', nb) p = (M.insert 'm' (fst (p ! 'm'), min (snd (p ! 'm')) nb) p, M.insert 'm' (max (fst (p ! 'm')) (nb + 1), snd (p ! 'm')) p)
        splitPart ('a', '<', nb) p = (M.insert 'a' (max (fst (p ! 'a')) nb, snd (p ! 'a')) p, M.insert 'a' (fst (p ! 'a'), min (snd (p ! 'a')) (nb - 1)) p)
        splitPart ('a', '>', nb) p = (M.insert 'a' (fst (p ! 'a'), min (snd (p ! 'a')) nb) p, M.insert 'a' (max (fst (p ! 'a')) (nb + 1), snd (p ! 'a')) p)
        splitPart ('s', '<', nb) p = (M.insert 's' (max (fst (p ! 's')) nb, snd (p ! 's')) p, M.insert 's' (fst (p ! 's'), min (snd (p ! 's')) (nb - 1)) p)
        splitPart ('s', '>', nb) p = (M.insert 's' (fst (p ! 's'), min (snd (p ! 's')) nb) p, M.insert 's' (max (fst (p ! 's')) (nb + 1), snd (p ! 's')) p)

        sizeOf :: Map Char (Int, Int) -> Int
        sizeOf p = product . map (\t -> snd t - fst t + 1) . filter (\p -> fst p <= snd p)$ map (p !) "xmas"
        solveW :: [Workflow] -> Map Char (Int, Int) -> Int
        solveW _ p
            | sizeOf p == 0 = 0
        solveW ((Accepted):l) p = (sizeOf p)
        solveW ((Rejected):l) p = 0
        solveW ((MoveTo s):l) p = solve wrkfls s p
        solveW ((Conditional c r):l) p = (solveW [r] valid) + (solveW l invalid)
            where
                (invalid, valid) = splitPart (fst c) p


part2 :: Input -> Int
part2 (Input w _) = solve w "in" (M.fromList [('x', (1, 4000)), ('m', (1, 4000)), ('a', (1, 4000)), ('s', (1, 4000))])


main :: IO ()
main = do
    content <- readFile "input.txt"
    let inp = parseInput content
    -- print inp
    print $ part1 inp
    print $ part2 inp
