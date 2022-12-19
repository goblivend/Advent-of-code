module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils
import Debug.Trace



data BluePrint = BluePrint {costOreOre :: Int, costOreClay :: Int, costOreObsi :: Int, costClayObsi :: Int, costOreGeode :: Int, costObsiGeode :: Int} deriving (Eq, Ord, Read, Show)
data Possibility = Possibility {nbOre :: Int, nbClay :: Int, nbObsi :: Int, nbGeode :: Int, rbOre :: Int, rbClay :: Int, rbObsi :: Int, rbGeode :: Int} deriving (Eq, Ord, Read, Show)


parseLine :: [String] -> BluePrint
parseLine line = BluePrint (readAt 6) (readAt 12) (readAt 18) (readAt 21) (readAt 27) (readAt 30)
    where
        readAt n = read (line !! n) :: Int

increaseNb :: Possibility -> Possibility -> Possibility
increaseNb old new = new {nbOre = nbOre new + rbOre old, nbClay = nbClay new + rbClay old, nbObsi = nbObsi new + rbObsi old, nbGeode = nbGeode new + rbGeode old}

getNewPossibilities :: BluePrint -> Possibility -> [Possibility]
getNewPossibilities bp pos = map (increaseNb pos) filteredNewPos
    where
        maxOre  = div (nbOre pos) (costOreOre bp)
        maxClay = div (nbOre pos) (costOreClay bp)
        maxObsi  = min (div (nbOre pos) (costOreObsi bp))  (div (nbClay pos) (costClayObsi bp))
        maxGeode = min (div (nbOre pos) (costOreGeode bp)) (div (nbObsi pos) (costObsiGeode bp))
        patterns = [(newOre, newClay, newObsi, newGeode) | newOre <- [0..maxOre], newClay <- [0..maxClay], newObsi <- [0..maxObsi], newGeode <- [0..maxGeode]]
        newPos = map (\news -> Possibility (getNewOre news) (getNewClay news) (getNewObsi news) (nbGeode pos) (getRbOre news) (getRbClay news) (getRbObsi news) (getRbGeode news)) patterns
        getNewOre (newOre, newClay, newObsi, newGeode) = getNew (costOreGeode bp) (newGeode) $ getNew (costOreObsi bp) (newObsi) $ getNew (costOreClay bp) (newClay) $ getNew (costOreOre bp) (newOre) (nbOre pos)
        getNewClay (newOre, newClay, newObsi, newGeode) = getNew (costClayObsi bp) (newObsi) (nbClay pos)
        getNewObsi (newOre, newClay, newObsi, newGeode) = getNew (costObsiGeode bp) (newGeode) (nbObsi pos)
        getNew cost nb old = old - cost * nb
        getRbOre   (newOre, _, _, _)   = newOre   + rbOre   pos
        getRbClay  (_, newClay, _, _)  = newClay  + rbClay  pos
        getRbObsi  (_, _, newObsi, _)  = newObsi  + rbObsi  pos
        getRbGeode (_, _, _, newGeode) = newGeode + rbGeode pos
        filteredNewPos = nub $ filter (\p -> nbOre p >= 0 && nbClay p >= 0 && nbObsi p >= 0) newPos


getNbGeode :: BluePrint -> Int -> [Possibility] -> Int
getNbGeode _ 0 possibilities = maximum . map (nbGeode) $ possibilities
getNbGeode bp timeLeft possibilities = trace (show timeLeft) (getNbGeode bp (timeLeft - 1) newPossibilities)
    where
        newPossibilities = nub . concat . map (trace ("Calling getNewPossibilities for " ++ show timeLeft ++ " with " ++ show (length possibilities)) (getNewPossibilities bp)) $ possibilities

main :: IO ()
main = do
    content <- readFile "shortinput.txt"
    let bluePrints = (parseLine . words) <$> lines content

    print bluePrints

    print $ sum $ (\(bp, nb) -> nb * getNbGeode bp 24 [Possibility 0 0 0 0 1 0 0 0]) <$> zip bluePrints [1..]
