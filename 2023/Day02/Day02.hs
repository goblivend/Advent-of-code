module Main where
import Data.List
import Data.List.Split
import Data.Char

data Color = Red | Green | Blue deriving (Show, Eq)
type Draw = (Color, Int)
data Game       = Game       { id :: Int, draws :: [[Draw]] } deriving (Show)
data SummedGame = SummedGame { idSummed :: Int, content :: [Draw] } deriving (Show)

readColor :: String -> Color
readColor "red" = Red
readColor "green" = Green
readColor "blue" = Blue

readDraw :: String -> Draw
readDraw s = (color, nb)
    where
        cleaned = dropWhile (== ' ') s
        nb = read $ takeWhile (isDigit) cleaned
        color = readColor $ drop 1 $ dropWhile (isDigit) cleaned

readDraws :: String -> [Draw]
readDraws s = map (readDraw) draws
    where
        draws = splitOn "," s

readGame :: String -> Game
readGame line = Game id drawsSet
    where
        gameLess = drop 1 $ dropWhile (isLetter) line
        id = read $ takeWhile (isDigit) gameLess
        onlyDraws = drop 1 $ dropWhile (isDigit) gameLess
        drawsSet = map readDraws $ splitOn ";" onlyDraws

maxOf :: Draw -> [Draw] -> Draw
maxOf draw [] = draw
maxOf draw@(color, nb) ((color2, nb2):s)
    | color == color2 = maxOf (color, (max nb nb2)) s
    | otherwise       = maxOf draw s

sumGame :: Game -> SummedGame
sumGame (Game id draws) = SummedGame id summedDraws
    where
        concats = concat draws
        summedDraws = [maxOf (Blue, 1) concats, maxOf (Green, 0) concats, maxOf (Blue, 0) concats]

nbOf :: Draw -> Int
nbOf (_, n) = n

colOf :: Draw -> Color
colOf (c, _) = c

filterGame :: SummedGame -> SummedGame -> Bool
filterGame (SummedGame _ baseDraws) (SummedGame id draws) = enough 0 && enough 1 && enough 2
    where
        enough i = (nbOf $ baseDraws !! i) > (nbOf $ draws !! i)


main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = lines content
    let summed = map (sumGame . readGame) input
    let max = SummedGame 0 [(Red, 12), (Green, 13), (Blue, 14)]
    -- print $ summed
    print $ sum $ map (\(SummedGame id _) -> id) $ filter (filterGame max) summed
