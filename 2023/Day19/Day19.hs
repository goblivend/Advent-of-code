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


data Workflow = Accepted | Rejected | MoveTo {dst :: String} | Conditional {cond :: (Char, Int -> Bool), ifTrue :: Workflow} deriving (Show)
data Part = Part {x::Int, m::Int, a::Int, s::Int} deriving (Show, Read, Eq, Ord)

data Input = Input {workflows :: Map String [Workflow], parts :: [Part]} deriving (Show)

instance Show (Int -> Bool) where
  show f = "<function>"



parseInput :: String -> Input
parseInput s = Input workflws prts
    where
        readCond :: String -> (Char, Int -> Bool)
        readCond ('x':'<':nb) = ('x', (< (read nb)))
        readCond ('x':'>':nb) = ('x', (> (read nb)))
        readCond ('m':'<':nb) = ('a', (< (read nb)))
        readCond ('m':'>':nb) = ('a', (> (read nb)))
        readCond ('a':'<':nb) = ('m', (< (read nb)))
        readCond ('a':'>':nb) = ('m', (> (read nb)))
        readCond ('s':'<':nb) = ('s', (< (read nb)))
        readCond ('s':'>':nb) = ('s', (> (read nb)))

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
            | fst c == 'x' && (snd c) (x p) = acceptFlow [r]
            | fst c == 'm' && (snd c) (m p) = acceptFlow [r]
            | fst c == 'a' && (snd c) (a p) = acceptFlow [r]
            | fst c == 's' && (snd c) (s p) = acceptFlow [r]
            | otherwise = acceptFlow l

part1 :: Input -> Int
part1 (Input wkfs prts) = sum . map (\p -> (x p) + m p + a p + s p) $  filter (accept wkfs "in") prts

solve :: Map String [Workflow] -> String -> Map Char [Int] -> Int
solve wrkfls curr p = solveW (wrkfls ! curr) p
    where
        splitPart :: (Char, Int -> Bool) -> Map Char [Int] -> (Map Char [Int], Map Char [Int])
        splitPart ('x', f) p = (M.insert 'x' (filter (not . f) (p ! 'x')) p, M.insert 'x' (filter f (p ! 'x')) p)
        splitPart ('m', f) p = (M.insert 'm' (filter (not . f) (p ! 'm')) p, M.insert 'm' (filter f (p ! 'm')) p)
        splitPart ('a', f) p = (M.insert 'a' (filter (not . f) (p ! 'a')) p, M.insert 'a' (filter f (p ! 'a')) p)
        splitPart ('s', f) p = (M.insert 's' (filter (not . f) (p ! 's')) p, M.insert 's' (filter f (p ! 's')) p)

        sizeOf :: Map Char [Int] -> Int
        sizeOf p = product $ map (length . (p !)) "xmas"
        solveW :: [Workflow] -> Map Char [Int] -> Int
        solveW _ p
            | sizeOf p == 0 = 0
        solveW ((Accepted):l) p = (sizeOf p)
        solveW ((Rejected):l) p = 0
        solveW ((MoveTo s):l) p = solve wrkfls s p
        solveW ((Conditional c r):l) p = (solveW [r] valid) + (solveW l invalid)
            where
                (invalid, valid) = splitPart (c) p


part2 :: Input -> Int
part2 (Input w _) = solve w "in" (M.fromList [('x', [1..4000]), ('m', [1..4000]), ('a', [1..4000]), ('s', [1..4000])])

main :: IO ()
main = do
    content <- readFile "input.txt"
    let inp = parseInput content
    -- print inp
    print $ part1 inp
    print $ part2 inp
