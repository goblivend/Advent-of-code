
module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils
import Debug.Trace

data Tile = Wall | Valley { bN::Bool, bS::Bool, bE::Bool, bW::Bool }  deriving (Eq, Ord, Read, Show)

type Board = [[Tile]]
type Pos = (Int, Int)

parseElt :: Char -> Tile
parseElt '#' = Wall
parseElt '.' = Valley False False False False
parseElt '^' = Valley True  False False False
parseElt 'v' = Valley False True  False False
parseElt '>' = Valley False False True  False
parseElt '<' = Valley False False False True

prettyShow :: Tile -> String
prettyShow Wall = "#"
prettyShow (Valley bN bS bE bW)
    | not bN && not bS && not bE && not bW = "."
    |     bN && not bS && not bE && not bW = "^"
    | not bN &&     bS && not bE && not bW = "v"
    | not bN && not bS &&     bE && not bW = "<"
    | not bN && not bS && not bE &&     bW = ">"
    | otherwise = show $ length $ filter (id) [bN, bS, bE, bW]

blankSpace :: Tile
blankSpace = Valley False False False False

blankBoard :: Board -> Board
blankBoard b = ((\e -> if e == Wall then Wall else blankSpace) <$>) <$> b

accessTuple :: [[a]] -> (Int, Int) -> a
accessTuple board (x, y) = board !! y !! x

replaceAt :: Int -> a -> [a] -> [a]
replaceAt x val l = take x l ++ [val] ++ drop (x+1) l

replaceXY :: Pos -> a -> [[a]] -> [[a]]
replaceXY (x, y) val l = replaceAt y (replaceAt x val (l !! y)) l

addTuple :: Pos -> Pos -> Pos
addTuple (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

addTuples :: Pos -> [Pos] -> [Pos]
addTuples pos poss = map (addTuple pos) poss

directions :: [Pos]
directions = [(0, -1), (0, 1), (1, 0), (-1, 0)]


setBliz :: Int -> Board -> Pos -> Board
setBliz 0 board (x, y) = replaceXY (x, y) ((board !! y !! x) {bN = True}) board
setBliz 1 board (x, y) = replaceXY (x, y) ((board !! y !! x) {bS = True}) board
setBliz 2 board (x, y) = replaceXY (x, y) ((board !! y !! x) {bE = True}) board
setBliz 3 board (x, y) = replaceXY (x, y) ((board !! y !! x) {bW = True}) board


updateBliz :: Int -> Board -> Pos -> Board
updateBliz dir board (x, y) = setBliz dir board newCo
    where
        addCo =  addTuple (directions !! dir) (x, y)
        newCo = if accessTuple board addCo == Wall then getCo dir else addCo
        getCo 0 = (x, length board - 2)
        getCo 1 = (x, 1)
        getCo 2 = (1, y)
        getCo 3 = (length (board !! 0) - 2, y)


updateSpot :: Board -> Board -> (Int, Int) -> Board
updateSpot old new (x, y) 
    | Wall == accessTuple old (x, y) = new
    | otherwise = foldl (upd) new [0..3]
        where
            upd b 0 = if bN (old !! y !! x) then updateBliz 0 b (x, y) else b
            upd b 1 = if bS (old !! y !! x) then updateBliz 1 b (x, y) else b
            upd b 2 = if bE (old !! y !! x) then updateBliz 2 b (x, y) else b
            upd b 3 = if bW (old !! y !! x) then updateBliz 3 b (x, y) else b

update :: Board -> Board -> Board
update blank board = foldl (updateSpot board) blank [(x, y) | x <- [0..length (board !! 0) - 1], y <- [0..length board - 1]]

hasBliz :: Tile -> Bool
hasBliz (Valley bN bS bE bW) = bN || bS || bE || bW
hasBliz _ = False

filterOutWalls :: Board -> [Pos] -> [Pos]
filterOutWalls board poss = filter (\p -> Wall /= accessTuple board p) poss

filterOutBliz :: Board -> [Pos] -> [Pos]
filterOutBliz board poss = filter (not . hasBliz . accessTuple board) poss

filterOutRange :: Board -> [Pos] -> [Pos]
filterOutRange board poss = filter (\(x, y) -> 0 <= x && 0 <= y && y < length board && x < length (board !! y)) poss


getMax :: Board -> Board -> [Pos] -> Pos -> (Board, Int)
getMax blank board poss end 
    | any (==end) poss = (board, 0)
    | otherwise = (finalB, res + 1)
        where
            newB = update blank board
            allPoss = concat $ map (\p -> addTuples p ((0, 0):directions)) poss
            uniquePoss = nub allPoss
            inRangePoss = {-trace ("Filtering range " ++ show uniquePoss)-} (filterOutRange board uniquePoss)
            valleyPoss = {-trace ("Filtering walls out " ++ show inRangePoss)-} (filterOutWalls board inRangePoss)
            newPoss = {-trace ("Filtering blizz out " ++ show valleyPoss)-} (filterOutBliz newB valleyPoss)
            (finalB, res) = {-trace (show $ length newPoss)-} (getMax blank newB newPoss end)



main :: IO ()
main = do
    content <- readFile "input.txt"
    let board = ((parseElt) <$>) <$> lines content
    let prettyPrint b = putStrLn $ unlines $ map concat $ (prettyShow <$>) <$> b
    -- prettyPrint board
    -- prettyPrint $ blankBoard board
    let getNoWall y = filter (\p -> Wall /= accessTuple board p) [(x, y) | x <- [0..length (board !! 0) - 1]]
    let start = head $ getNoWall 0
    let end = head $ getNoWall (length board - 1)
    
    let blank = blankBoard board

    let (bAfter1, res1) = getMax blank board [start] end
    putStrLn $ "Part 1:" ++ show res1
    let (bAfter2, res2) = getMax blank bAfter1 [end] start
    let (bAfter3, res3) = getMax blank bAfter2 [start] end
    putStrLn $ "Part 2:" ++ show (res1 + res2 + res3 )
