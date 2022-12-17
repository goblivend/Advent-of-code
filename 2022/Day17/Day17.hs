module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils
import Debug.Trace

data Jet = JRight | JLeft deriving (Eq, Ord)

instance Show Jet where
    show JLeft = "<"
    show JRight = ">"

instance Read Jet where
    readsPrec _ ('<':r) = [(JLeft, r)]
    readsPrec _ ('>':r) = [(JRight, r)]

data Rock = Void | Fall | Fixed deriving (Eq, Ord)

instance Show Rock where
    show Void = "."
    show Fall = "@"
    show Fixed = "#"

instance Read Rock where
    readsPrec _ ('.':r) = [(Void, r)]
    readsPrec _ ('@':r) = [(Fall, r)]
    readsPrec _ ('#':r) = [(Fixed, r)]

rocks = map (map (map (\r -> read [r] :: Rock)) . lines) ["..@@@@.", "...@...\n..@@@..\n...@...", "....@..\n....@..\n..@@@..", "..@....\n..@....\n..@....\n..@....", "..@@...\n..@@..."]
emptyLine = (map (\r -> read [r] :: Rock)) "......."

cleanMap :: [[Rock]] -> [[Rock]]
cleanMap [] = []
cleanMap (l:rest)
    | l == emptyLine = cleanMap rest
    | otherwise = l:rest

lock :: [[Rock]] -> [[Rock]]
lock mp = map (map (\r -> if r == Fall then Fixed else r)) mp

isValid :: [[Rock]] -> Int -> Int -> Bool
isValid mp x y = 0 <= x && x < 7 && y < length mp && mp !! y !! x /= Fixed

insertRock :: Int -> [[Rock]] -> [[Rock]]
insertRock n mp = (rocks !! (n `mod` (length rocks))) ++ replicate 3 emptyLine ++ cleanMap mp

getDir :: Jet -> Int
getDir j
    | j == JLeft = -1
    | otherwise = 1

replaceAt :: Int -> a -> [a] -> [a]
replaceAt x val l = take x l ++ [val] ++ drop (x+1) l

replaceXY :: Int -> Int -> a -> [[a]] -> [[a]]
replaceXY x y val l = replaceAt y (replaceAt x val (l !! y)) l

getYFall :: [[Rock]] -> [Int]
getYFall mp = filter (\y -> elem Fall (mp !! y)) [0..length mp - 1]

isAllValid :: [[Rock]] -> Int -> Int -> Bool
isAllValid mp dx dy = all (\(x, y) -> isValid mp (x+dx) (y+dy)) co
    where
        ys = getYFall mp
        co = concat $ map (\y -> map (\x -> (x, y)) $ elemIndices Fall (mp !! y))  ys

fall :: Int -> Int -> Int -> Int -> [[Rock]] -> [[Rock]]
fall x y dx dy mp = replaceXY (x+dx) (y+dy) Fall $ replaceXY x y Void mp

fallLine ::  Int -> Int -> ([Int] -> [Int]) -> [[Rock]] -> Int -> [[Rock]]
fallLine dx dy sortMe mp y = newMp
    where
        xs = sortMe $ elemIndices Fall (mp !! y)
        co = zip xs $ repeat y
        newMp = foldl (\mp (x, y) -> fall x y dx dy mp) mp co

fallOneLine :: [[Rock]] -> Int -> Int -> [[Rock]]
fallOneLine mp dx dy= newMp
    where
        ys = reverse . sort $ getYFall mp
        sortMe = if dx == -1 then sort else reverse . sort
        newMp = foldl (fallLine dx dy sortMe) mp ys

makeFallOne :: [Jet] -> Int -> [[Rock]] -> (Int, [[Rock]])
makeFallOne jets nbJet mp = (newNbJet, newMp)
    where
        dir = (getDir $ jets !! nbJet)
        sideMp = if isAllValid mp dir 0 then fallOneLine mp dir 0 else mp
        (newNbJet, newMp) = if not $ isAllValid sideMp 0 1 then (nbJet+1, lock sideMp) else makeFallOne jets (nbJet+1) $ fallOneLine sideMp 0 1

shorttenMp :: [[Rock]] -> [Bool] -> [Bool] -> (Int, [[Rock]])
shorttenMp [] _ _ = (0, [])
shorttenMp (l:mp) prevSeen seen
    | all (\(ps, s, r) -> ps || s || r==Fixed) (zip3 prevSeen seen l) = (length mp, [l])
    | otherwise = (ln, l:newMp)
        where
            (ln, newMp) = shorttenMp mp seen (map (==Fixed) l)


makeFallAll :: Int -> [Jet] -> Int -> Int -> [[Rock]] -> Int
makeFallAll nbRocks jets nbJet currRock mp
    | nbRocks <= currRock = length mp
    | otherwise = len
    where
        insertedMp = {-trace ("Inserting " ++ show currRock)-} (insertRock currRock mp)
        (newNbJet, fallenMp) = {-trace ("Made one fall, jets at : "++ show nbJet)-} (makeFallOne jets nbJet insertedMp)
        (cleanLen, cleanMp) = {-trace ("Cleaning")-} shorttenMp (cleanMap fallenMp) (replicate 7 False) (replicate 7 False)

        len = cleanLen + makeFallAll nbRocks jets newNbJet (currRock + 1) (trace (show (length mp)) (cleanMp))


main :: IO ()
main = do
    content <- readFile "input.txt"
    let jets = map (\j -> read [j] :: Jet) . head $ lines content
    let infJets = concat . repeat $ jets

    -- let (endN, newMp) = makeFallOne jets 0 mp
    -- pretty newMp
    -- let (endN2, newMp2) = makeFallOne jets (endN+4) $ insertRock 2 newMp
    -- pretty newMp2
    print $  makeFallAll 10000 infJets 0 0 []
    -- print $  makeFallAll 1000000000000 infJets 0 0 []
    -- print $ length $ makeFallAll 2022 infJets 0 0 []
    where
        pretty mp = putStrLn $ unlines $ map (concat . map show ) mp
