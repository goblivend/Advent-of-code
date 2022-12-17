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

insertRock :: Int -> [[Rock]] -> [[Rock]]
insertRock n map = (rocks !! (n `mod` 5)) ++ replicate 3 emptyLine ++ cleanMap map

fallOneLine :: Jet -> [[Rock]] -> [[Rock]]

makeFall :: [Jet] -> Int -> [[Rock]] -> (Int, [[Rock]])
makeFall jets n map =


main :: IO ()
main = do
    content <- readFile "shortinput.txt"
    let jets = map (\j -> read [j] :: Jet) . head $ lines content
    let infJets = concat . repeat $ jets
    print $ take 100 infJets

    print $ insertRock 0 $ insertRock 2 []
