module Main where
import Data.List
import Data.List.Split
import Data.Char

data Color = Red | Green | Blue deriving (Show, Eq)
type Draw = (Color, Int)
data Game       = Game       { id :: Int, draws :: [[Draw]] }
data SummedGame = SummedGame { idSummed :: Int, content :: [Draw] }

readColor :: String -> Color
readColor "red" = Red
readColor "green" = Green
readColor "blue" = Blue

readDraw :: String -> Draw
readDraw s = (color, nb)
    where
        nb = read $ takeWhile (isDigit) s
        color = readColor $ drop 1 $ dropWhile (isDigit) s

readDraws :: String -> [Draw]
readDraws s = map (readDraw . dropWhile (== ' ')) $ splitOn "," s

readGame :: String -> Game
readGame line = Game id drawsSet
    where
        gameLess = drop 1 $ dropWhile (isLetter) line
        id = read $ takeWhile (isDigit) gameLess
        onlyDraws = drop 1 $ dropWhile (isDigit) gameLess
        drawsSet = map readDraws $ splitOn ";" onlyDraws

maxDraw :: Draw -> [Draw] -> Draw
maxDraw draw [] = draw
maxDraw draw@(color, nb) ((color2, nb2):s)
    | color == color2 = maxDraw (color, (max nb nb2)) s
    | otherwise       = maxDraw draw s

maxDraws :: [Draw] -> [Draw]
maxDraws draws = map (\c -> maxDraw (c, 0) draws) [Red, Green, Blue]

sumGame :: Game -> SummedGame
sumGame (Game id draws) = SummedGame id $ maxDraws $ concat draws

powerGame :: Game -> Int
powerGame (Game id draws) = foldr (*) 1 $ map (\(_, n) -> n) $ maxDraws $ concat draws

-- Draws are always ordered as [Red, Green, Blue] so no need to check color when accesing the index
filterGame :: [Draw] -> SummedGame -> Bool
filterGame baseDraws (SummedGame id draws) = enough 0 && enough 1 && enough 2
    where
        nbOf (_, n) = n
        enough i = (nbOf $ baseDraws !! i) >= (nbOf $ draws !! i)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = lines content
    let games = map (readGame) input
    let max = [(Red, 12), (Green, 13), (Blue, 14)]
    print $ sum $ map (\(SummedGame id _) -> id) $ filter (filterGame max) $ map (sumGame) games
    print $ sum $ map (powerGame) games
