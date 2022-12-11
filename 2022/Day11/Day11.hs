module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils

--data Monkey = Monkey { activity :: Int, items :: [Int], worry :: (Int -> Int), decision ::(Int -> Int) }

parseOperation :: String -> (Int -> Int)
parseOperation ope
    | first == "old" && not (second == "old") = (\old -> (op sign) old (read second :: Int))
    | first == "old" && second == "old" = (\old -> (op sign) old old)
    | not (first == "old") && second == "old" = (\old -> (op sign) (read first :: Int) old)
    | otherwise = (\old -> (op sign) (read first :: Int) (read second :: Int))
    where
        first = (splitOn " " ope) !! 0
        op "*" = (*)
        op "/" = (div)
        op "+" = (+)
        op "-" = (-)
        sign = (splitOn " " ope) !! 1
        second = (splitOn " " ope) !! 2

parseMonkey :: String -> (Int, [Int], (Int -> Int), (Int -> Int))
parseMonkey content = (0, items, worryLevel, decision)
    where
        lns = lines content
        items = map (\e -> read e :: Int) $ splitOn ", "$ (splitOn ": " (lns !! 1)) !! 1
        worryLevel = parseOperation ((splitOn " = " (lns !! 2)) !! 1)
        decision = makeDecision (read (last $ words $ lns !! 4) :: Int) (read (last $ words $ lns !! 5) :: Int) (read (last $ words $ lns !! 3) :: Int)

getActivity :: (Int, [Int], Int -> Int, Int -> Int) -> Int
getActivity (acti, items, worryLevel, makeDecision) = acti

getItems :: (Int, [Int], Int -> Int, Int -> Int) -> [Int]
getItems (acti, items, worryLevel, makeDecision) = items

getWorry :: (Int, [Int], Int -> Int, Int -> Int) -> (Int -> Int)
getWorry (acti, items, worryLevel, makeDecision) = worryLevel

getDecision :: (Int, [Int], Int -> Int, Int -> Int) -> (Int -> Int)
getDecision (acti, items, worryLevel, makeDecision) = makeDecision

makeDecision :: Int -> Int -> Int -> Int -> Int
makeDecision mkT mkF modulo worry
    | mod worry modulo == 0 = mkT
    | otherwise = mkF

putItem :: [(Int, [Int], Int -> Int, Int -> Int)] -> Int -> Int -> [(Int, [Int], Int -> Int, Int -> Int)]
putItem monkeys monkey worry = take monkey monkeys ++ [newMonkey] ++ drop (monkey + 1) monkeys
    where
        currMonkey = monkeys !! monkey
        newMonkey = (getActivity currMonkey, getItems currMonkey ++ [worry], getWorry currMonkey, getDecision currMonkey)

throwItems :: Int -> [(Int, [Int], Int -> Int, Int -> Int)] -> Int -> [(Int, [Int], Int -> Int, Int -> Int)]
throwItems mkBusiness monkeys monkey
    | getItems currMonkey == [] = monkeys
    | otherwise = throwItems mkBusiness newMonkeys monkey
        where
            currMonkey = monkeys !! monkey
            worry = div (getWorry currMonkey $ head $ getItems currMonkey) mkBusiness
            newMonkey = (1 + getActivity currMonkey, tail (getItems currMonkey), getWorry currMonkey, getDecision currMonkey)
            monkeysWithItem = putItem monkeys ((getDecision currMonkey) worry) worry
            newMonkeys = take monkey monkeysWithItem ++ [newMonkey] ++ drop (monkey + 1) monkeysWithItem

aRound :: Int -> [(Int, [Int], Int -> Int, Int -> Int)] -> Int -> [(Int, [Int], Int -> Int, Int -> Int)]
aRound mkBusiness monkeys monkey
    | monkey >= length monkeys = monkeys
    | otherwise = aRound mkBusiness newMonkeys (monkey + 1)
        where
            newMonkeys = throwItems mkBusiness monkeys monkey

makeRound :: Int -> Int -> [(Int, [Int], Int -> Int, Int -> Int)] -> [(Int, [Int], Int -> Int, Int -> Int)]
makeRound 0 _ monkeys = monkeys
makeRound nbround mkBusiness monkeys = makeRound (nbround - 1) mkBusiness $ aRound mkBusiness monkeys 0

main :: IO ()
main = do
    content <-  readFile "shortinput.txt"
    let monkeys = map parseMonkey $ splitOn "\n\n" content
    --print monkeys
    --print $ getActi $ monkeys !! 0
    print $ product $ take 2 $ reverse $ sort $ map getActivity $ makeRound 20 3 $ monkeys
    print $ product $ take 2 $ reverse $ sort $ map getActivity $ makeRound 10000 1 $ monkeys



-- Monkey :
-- Items   Worry level Decision according to level
-- ([Int], (Int -> Int), (Int -> Int))
-- Worry / 3 after inspection
-- Decision :
-- makeDecision mkT mkF modulo worry =
