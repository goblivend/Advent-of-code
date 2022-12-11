module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils

data Monkey = Monkey { activity :: Int, items :: [Int], worryLevel :: (Int -> Int), decision ::(Int -> Int), modulo :: Int }

parseOperation :: String -> (Int -> Int)
parseOperation ope
    | first == "old" && not (second == "old") = (\old -> (op sign) old (read second :: Int))
    | first == "old" && second == "old" = (\old -> (op sign) old old)
    | not (first == "old") && second == "old" = (\old -> (op sign) (read first :: Int) old)
    | otherwise = (\old -> (op sign) (read first :: Int) (read second :: Int))
    where
        first = (splitOn " " ope) !! 0
        sign = (splitOn " " ope) !! 1
        second = (splitOn " " ope) !! 2
        op "*" = (*)
        op "/" = (div)
        op "+" = (+)
        op "-" = (-)

makeDecision :: Int -> Int -> Int -> Int -> Int
makeDecision mkT mkF modulo worry
    | mod worry modulo == 0 = mkT
    | otherwise = mkF

parseMonkey :: String -> Monkey
parseMonkey content = Monkey 0 items worryLevel decision modulo
    where
        lns = lines content
        items = map (\e -> read e :: Int) $ splitOn ", " $ (splitOn ": " (lns !! 1)) !! 1
        worryLevel = parseOperation ((splitOn " = " (lns !! 2)) !! 1)
        modulo = (read (last $ words $ lns !! 3) :: Int)
        decision = makeDecision (read (last $ words $ lns !! 4) :: Int) (read (last $ words $ lns !! 5) :: Int) modulo

putItem :: [Monkey] -> Int -> Int -> [Monkey]
putItem monkeys monkey worry = take monkey monkeys ++ [newMonkey] ++ drop (monkey + 1) monkeys
    where
        currMonkey = monkeys !! monkey
        newMonkey = currMonkey {items = items currMonkey ++ [worry]}

throwItems :: Int -> [Monkey] -> Int -> [Monkey]
throwItems mkBusiness monkeys monkey
    | items currMonkey == [] = monkeys
    | otherwise = throwItems mkBusiness newMonkeys monkey
        where
            currMonkey = monkeys !! monkey
            worry = (div (worryLevel currMonkey $ head $ items currMonkey) mkBusiness)
            newMonkey = currMonkey {activity = 1 + activity currMonkey, items = tail (items currMonkey)}
            commonMultiplier = product $ map modulo monkeys
            monkeysWithItem = putItem monkeys ((decision currMonkey) worry) (mod worry commonMultiplier)
            newMonkeys = take monkey monkeysWithItem ++ [newMonkey] ++ drop (monkey + 1) monkeysWithItem

aRound :: Int -> [Monkey] -> Int -> [Monkey]
aRound mkBusiness monkeys monkey
    | monkey >= length monkeys = monkeys
    | otherwise = aRound mkBusiness newMonkeys (monkey + 1)
        where
            newMonkeys = throwItems mkBusiness monkeys monkey

makeRound :: Int -> Int -> [Monkey] -> [Monkey]
makeRound 0 _ monkeys = monkeys
makeRound nbround mkBusiness monkeys = makeRound (nbround - 1) mkBusiness $ aRound mkBusiness monkeys 0

main :: IO ()
main = do
    content <-  readFile "input.txt"
    let monkeys = map parseMonkey $ splitOn "\n\n" content
    print $ product $ take 2 $ reverse $ sort $ map activity $ makeRound 20 3 $ monkeys
    print $ product $ take 2 $ reverse $ sort $ map activity $ makeRound 10000 1 $ monkeys
