module Main where

import Data.Char
import Data.Maybe ( Maybe, fromJust, isJust )
import Debug.Trace(trace)
import Data.Tuple
import Data.Tuple.Extra;
import Control.Parallel.Strategies
import Data.List
import Data.List.Unique
import Data.Set (Set)
import Data.Map (Map, (!))
-- import Data.NumInstances.Tuple
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

data Input = Input {keys :: Set String, mp :: Map String (Set String)} deriving (Show)
data Tuple = Tuple {k1 :: String, k2 :: String} deriving (Ord)

instance Eq Tuple where
    t1 == t2 = (k1 t1 == k1 t2 && k2 t1 == k2 t2) ||
               (k2 t1 == k1 t2 && k1 t1 == k2 t2)

parseInput :: String -> Input
parseInput inp = Input (S.fromList . concat . map (\(k, v) -> k:S.toList v)$ M.toList mp) mp
    where

        mp = M.fromList . map parseLine . lines $ inp
        parseLine s = (name, S.fromList links)
            where
                name = take 3 s
                links = words $ drop 5 s

toUndirected :: Input -> Input
toUndirected (Input keys mp) = Input keys $ fill (M.toList mp) M.empty
    where
        fill [] m = m
        fill ((k, v):l) m = fill l $  foldl (\m k' -> M.insertWith (S.union) k' (S.singleton k) $ M.insertWith  (S.union) k (S.singleton k') m) m v

splitWires :: Input -> [Tuple] -> Input
splitWires (Input keys mp) []          = Input keys mp
splitWires (Input keys mp) ((Tuple k k'):l) = splitWires (Input keys newMp) l
    where
        newMp = M.update (Just . S.delete k') k . M.update (Just . S.delete k) k' $ mp

sizesSplit :: Input -> Int
sizesSplit (Input keys mp) =  (visited * (S.size keys - visited))

    where
        visited = myvisit "ljm" -- "bvb"
        myvisit k = visit (S.empty) (S.singleton k)
        visit seen ks
            | S.null ks = -1
            | otherwise = length ks + visit (S.union seen newOnes) (newOnes)
                where
                    newOnes = S.filter (`S.notMember` seen) . foldl S.union S.empty $ S.map (mp!) ks



part1 :: Input -> Int
part1 inp = sizesSplit . splitWires undir $ splits
    where
        undir = toUndirected inp
        -- splits = [Tuple "hfx" "pzl", Tuple "bvb" "cmg", Tuple "nvd" "jqt"]
        splits = [Tuple "jkn" "cfn", Tuple "ljm" "sfd", Tuple "gst" "rph"]

graphStr inp = unlines . map (\(Tuple k1 k2) -> k1 ++ " -- " ++ k2) . S.toList . S.fromList $ [Tuple k1 k2 | k1 <-(S.toList keys), k2<-(S.toList (mp ! k1)), k1 < k2]
    where
        (Input keys mp) = toUndirected inp



main :: IO ()
main = do
    content <- readFile "input.txt"
    let inp = parseInput content
    -- putStr $ graphStr inp
    print $ part1 inp
