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

rocks = cycle $ map (map (map (\r -> read [r] :: Rock)) . lines) ["..@@@@.", "...@...\n..@@@..\n...@...", "....@..\n....@..\n..@@@..", "..@....\n..@....\n..@....\n..@....", "..@@...\n..@@..."]
emptyLine =  (map (\r -> read [r] :: Rock)) "......."
emptyLines =  replicate 3 emptyLine

cleanMap :: [[Rock]] -> [[Rock]]
cleanMap [] = []
cleanMap (l:rest)
    | l == emptyLine = cleanMap rest
    | otherwise = l:rest

lock :: [[Rock]] -> [[Rock]]
lock mp = map (map (\r -> if r == Fall then Fixed else r)) mp

isValid :: [[Rock]] -> Int -> Int -> Bool
isValid mp x y = 0 <= x && x < 7 && y < length mp && mp !! y !! x /= Fixed

insertRock :: [[Rock]] -> [[Rock]] -> [[Rock]]
insertRock r mp = r ++ emptyLines ++ cleanMap mp

getDir :: Jet -> Int
getDir j
    | j == JLeft = -1
    | otherwise = 1

replaceAt :: Int -> a -> [a] -> [a]
replaceAt x val l = take x l ++ [val] ++ drop (x+1) l

replaceXY :: Int -> Int -> a -> [[a]] -> [[a]]
replaceXY x y val l = replaceAt y (replaceAt x val (l !! y)) l

getYFall :: [[Rock]] -> [Int]
getYFall mp = findIndices (\l -> elem Fall l) mp

isAllValid :: [[Rock]] -> Int -> Int -> Bool
isAllValid mp dx dy = all (\(x, y) -> isValid mp (x+dx) (y+dy)) co
    where
        ys = getYFall mp
        co = concatMap (\y -> map (\x -> (x, y)) $ elemIndices Fall (mp !! y))  ys

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

makeFallOne :: [Jet] -> [[Rock]] -> ([Jet], [[Rock]])
makeFallOne jets mp = (newJets, newMp)
    where
        dir = getDir $ head jets
        sideMp = if isAllValid mp dir 0 then fallOneLine mp dir 0 else mp
        (newJets, newMp) = if not $ isAllValid sideMp 0 1 then (tail jets, lock sideMp) else makeFallOne (tail jets) $ fallOneLine sideMp 0 1

shorttenMp :: [[Rock]] -> [Bool] -> [Bool] -> (Int, [[Rock]])
shorttenMp [] _ _ = (0, [])
shorttenMp (l:mp) prevSeen seen
    | all (\(ps, s, r) -> ps || s || r==Fixed) (zip3 prevSeen seen l) = (length mp, [l])
    | otherwise = (ln, l:newMp)
        where
            (ln, newMp) = shorttenMp mp seen (map (==Fixed) l)

emptySeen = replicate 7 False

makeFallAll :: Int -> [Jet] -> [[[Rock]]] -> [[Rock]] -> Int
makeFallAll nbRocks jets rocks mp
    | nbRocks <= 0 = length mp
    | otherwise = len
    where
        insertedMp = insertRock (head rocks) mp
        (newJets, fallenMp) =  makeFallOne jets insertedMp
        (cleanLen, cleanMp) =  shorttenMp (cleanMap fallenMp) emptySeen emptySeen
        len = cleanLen + makeFallAll (nbRocks-1) newJets  (tail rocks) cleanMp

main :: IO ()
main = do
    content <- readFile "input2.txt"
    -- let jets = cycle $ map (\j -> read [j] :: Jet) . head $ lines content
    print $ length . map (\j -> read [j] :: Jet) . head $ lines content
    -- print $  makeFallAll 100 jets rocks []
    -- print $  makeFallAll 1000 jets rocks []
    -- print $  makeFallAll 10000 jets rocks []
    -- print $  makeFallAll 100000 jets rocks []
    -- print $  makeFallAll 500000 jets rocks []
    -- print $  makeFallAll 1000000000000 infJets 0 0 []
    where
        pretty mp = putStrLn $ unlines $ map (concat . map show ) mp
