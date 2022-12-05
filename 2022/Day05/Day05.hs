module Main where

import Data.Char
import Data.List
import Data.List.Split

getTuple :: [String] -> (Int, Int, Int)
getTuple l = (read (l!!1), read (l!!3) - 1, read (l!!5) - 1)

crane9001 :: [[Char]] -> (Int, Int, Int) -> [[Char]]
crane9001 s (nb, src, dst)
    | nb == 0 = s
    | src < dst = (take (src) s) ++ [(drop nb (s!!src))] ++ (map (s!!) [src+1..dst-1]) ++ [(take nb (s!!src))++(s!!dst)] ++ (drop (dst+1) s)
    | src > dst = (take (dst) s) ++ [(take nb (s!!src))++(s!!dst)] ++ (map (s!!) [dst+1..src-1]) ++ [(drop nb (s!!src))] ++ (drop (src+1) s)
    | otherwise = s

crane9000 :: [[Char]] -> (Int, Int, Int) -> [[Char]]
crane9000 s (nb, src, dst)
    | nb == 0 = s
    | otherwise = crane9000 (crane9001 s (1, src, dst)) (nb-1, src, dst)

makeTheMoves :: ([[Char]] -> (Int, Int, Int) -> [[Char]]) -> [[Char]] -> [(Int, Int, Int)] -> [[Char]]
makeTheMoves crane s [] = s
makeTheMoves crane s (mv:mvs) = makeTheMoves crane (crane s mv) mvs


main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = map getTuple $ map words $ lines content
    print $ map head $ makeTheMoves crane9000 inputStack input
    print $ map head $ makeTheMoves crane9001 inputStack input


{-

    [B]             [B] [S]
    [M]             [P] [L] [B] [J]
    [D]     [R]     [V] [D] [Q] [D]
    [T] [R] [Z]     [H] [H] [G] [C]
    [P] [W] [J] [B] [J] [F] [J] [S]
[N] [S] [Z] [V] [M] [N] [Z] [F] [M]
[W] [Z] [H] [D] [H] [G] [Q] [S] [W]
[B] [L] [Q] [W] [S] [L] [J] [W] [Z]
 1   2   3   4   5   6   7   8   9

-}
inputStack = ["NWB","BMDTPSZL","RWZHQ","RZJVDW","BMHM","BPVHJNGL","SLDHFZQJ","BQGJFSW","JDCSMWZ"]
{-
    [D]
[N] [C]
[Z] [M] [P]
 1   2   3
-}

shortinputStack = ["NZ","DCM","P"]
