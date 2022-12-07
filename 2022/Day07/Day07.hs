module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils

data Tree = Leaf | Tree { name :: String, weight :: Int, children :: [Tree], parent :: Tree } deriving (Show)

createTree :: Tree -> [[String]] -> Tree
createTree t [] = t
createTree t (line::inst)
    | line !! 0 == "$" && line !! 1 == "cd" && line !! 2 == ".." = createTree t.parent inst
    | line !! 0 == "$" && line !! 1 == "cd" = createTree t inst



main :: IO ()
main = do
    content <-  readFile "input.txt"
    let instructions = map words $ lines content
    print instructions
    let l = Tree "root" 66 [] Leaf
    print l
