module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils
import Debug.Trace

data Monkey = Number { name :: String, nb :: Int } | Operation { name :: String,  mk1 :: String, mk2 :: String, op :: Char }

instance Show Monkey where
    show (Number nm nb) = "Nb : "  ++ nm ++ " : " ++ show nb
    show (Operation nm m1 m2 _) = "Op : " ++ nm ++ " : " ++ m1 ++ " " ++ m2

getop :: Char -> (Int -> Int -> Int)
getop '+' = (+)
getop '-' = (-)
getop '*' = (*)
getop '/' = (div)

parseLine :: String -> Monkey
parseLine l = monkey
    where
        name' = take 4 l
        l' = drop 6 l
        nbMonkey = Number name' (read l' :: Int)
        mk1' = take 4 l'
        op = head $ drop 5 l'

        mk2' = take 4 $ drop 7 l'
        opMonkey = Operation name' mk1' mk2' op
        monkey = if isDigit $ head l' then nbMonkey else opMonkey

isNb :: Monkey -> Bool
isNb (Number _ _) = True
isNb _ = False

replaceAt :: Int -> a -> [a] -> [a]
replaceAt x val l = take x l ++ [val] ++ drop (x+1) l

getMkIdByName :: [Monkey] -> String -> Int
getMkIdByName monkeys name' = head $ findIndices (\m -> name'== name m) monkeys

getMkByName :: [Monkey] -> String -> Monkey
getMkByName monkeys name' = monkeys !! getMkIdByName monkeys name'

getResultFor :: [Monkey] -> String -> [Monkey]
getResultFor monkeys name'
    | isNb $ getMkByName monkeys name' = monkeys
    | otherwise = replaceAt i (Number name' ((getop $ op mk) nb1 nb2)) mksFor2
        where
            i = getMkIdByName monkeys name'
            mk = monkeys !! i
            mksFor1 = getResultFor monkeys (mk1 mk)
            nb1 = nb $ getMkByName mksFor1 (mk1 mk)
            mksFor2 = getResultFor mksFor1 (mk2 mk)
            nb2 = nb $ getMkByName mksFor2 (mk2 mk)

getResOf :: Monkey -> Int
getResOf (Number _ nb) = nb
getResOf _ = trace ("Not a number") (-1)

getRouteTo :: [Monkey] -> String -> String -> [String]
getRouteTo monkeys curr target
    | target == curr = [target]
    | isNb $ getMkByName monkeys curr = []
    | otherwise = route
        where
            mk = getMkByName monkeys curr
            m1Route = getRouteTo monkeys (mk1 mk) target
            m2Route = getRouteTo monkeys (mk2 mk) target
            route = if m1Route == [] then if m2Route == [] then [] else curr:m2Route else curr:m1Route

solveOp :: Char -> Int -> Int -> Int -> Int
solveOp op res n nToSolve = solve op
    where
        solve '+' = res - n
        solve '*' = div res n
        solve '-' = if nToSolve == 1 then res + n else n - res
        solve '/' = if nToSolve == 1 then res * n else div n res
        solve '=' = n

guessInputOf :: [String] -> [Monkey] -> Int -> Int
guessInputOf [] monkeys acc = trace ("Should never happen, empty route") 1
guessInputOf (curr:route) monkeys acc
    | route == [] = acc
    | isNb $ getMkByName monkeys curr = trace ("Should never happen, nb in route") 0
    | otherwise = guessInputOf route newMonkeys newAcc
        where
            mk = getMkByName monkeys curr
            newMonkeys = if mk1 mk == head route then getResultFor monkeys (mk2 mk) else getResultFor monkeys (mk1 mk)
            newAcc = if mk1 mk == head route then solveOp (op mk) acc (nb $ getMkByName newMonkeys (mk2 mk)) 1 else solveOp (op mk) acc (nb $ getMkByName newMonkeys (mk1 mk)) 0

main :: IO ()
main = do
    content <- readFile "input.txt"
    let monkeys = parseLine <$> lines content

    print$ getResOf $ getMkByName (getResultFor monkeys "root") "root"

    let idRoot = getMkIdByName monkeys "root"
    let monkeys2 = replaceAt idRoot ((monkeys !! idRoot) { op = '='}) monkeys
    print $ guessInputOf (getRouteTo monkeys2 "root" "humn") monkeys2 0
