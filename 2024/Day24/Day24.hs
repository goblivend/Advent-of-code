module Main where

import Data.Bits
import Data.List
import Data.List.Split
import Data.List.Unique
import Data.Map (Map)
import Data.Map qualified as M
import Data.Matrix (Matrix, (!))
import Data.Matrix qualified as Mat
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple.Extra
import Debug.Trace
import System.Environment
import Text.Regex.TDFA ((=~))

-- TODO: Cleanup imports after day done

data BinOp = AND | OR | XOR deriving (Show, Eq)

data Operation = Operation {l :: String, r :: String, op :: BinOp} deriving (Show, Eq)

type Input = (Map String Bool, Map String Operation)

type Output = Int

parseInput :: String -> Input
parseInput input = (defaultValues, opMap)
  where
    [defaultStr, opStr] = splitOn "\n\n" input

    intToBool 0 = False
    intToBool _ = True

    parseDefault = (\[key, n] -> (key, intToBool $ read n)) . splitOn ": "
    defaultValues = M.fromList . map parseDefault $ lines defaultStr
    parseOp [lKey, "AND", rKey, _, oKey] = (oKey, Operation lKey rKey AND)
    parseOp [lKey, "OR", rKey, _, oKey] = (oKey, Operation lKey rKey OR)
    parseOp [lKey, "XOR", rKey, _, oKey] = (oKey, Operation lKey rKey XOR)
    opMap = M.fromList . map (parseOp . splitOn " ") $ lines opStr

wiresToInt :: String -> Map String Bool -> Output
wiresToInt prefix values = binToInt . map (\b -> if b then 1 else 0) . map snd . sortOn fst . filter (isPrefixOf prefix . fst) $ M.assocs values
  where
    binToInt = foldr (\c res -> res * 2 + c) 0

applyOp :: Map String Bool -> Operation -> Bool
applyOp values (Operation lKey rKey AND) = (values M.! lKey) && (values M.! rKey)
applyOp values (Operation lKey rKey OR) = (values M.! lKey) || (values M.! rKey)
applyOp values (Operation lKey rKey XOR) = (values M.! lKey) `xor` (values M.! rKey)

applyOps :: Input -> Map String Bool
applyOps (defaultValues, opMap) = foldl solve defaultValues opList
  where
    opList = filter (isPrefixOf "z") $ M.keys opMap

    solve v key
      | key `M.member` v = v
      | otherwise = newValues
      where
        (Operation lKey rKey o) = opMap M.! key
        v' = solve v lKey
        v'' = solve v' rKey

        res = applyOp v'' (opMap M.! key)
        newValues = M.insert key res v''

part1 :: Input -> Output
part1 = wiresToInt "z" . applyOps


intToWires :: Int -> Int -> [Bool]
intToWires size val = reverse . snd $ foldl (\(v, r) _ -> (v `div` 2, (v `mod` 2 == 1) :r)) (val, []) $ replicate size 0

badOutputs :: [Bool] -> Map String Bool -> [String]
badOutputs expected vals = map snd $ filter (\(e, s) -> e /= vals M.! s) $ zip expected zs
  where
    zs = sort . filter (isPrefixOf "z") $ M.keys vals

findInvalidGates :: Input -> Set String
findInvalidGates (defaultV, opMap) = S.unions . map badKeys $ zip values $ tail values
  where
    values = [2^i | i<-[0..44]] ++ [2^i-1 | i<-[0..44]] ++ [0..512]
    xs = filter (isPrefixOf "x") $ M.keys defaultV
    ys = filter (isPrefixOf "y") $ M.keys defaultV
    xyLength = length xs
    valuesFrom x y = M.fromList $ (zip xs $ intToWires xyLength x) ++ (zip ys $ intToWires xyLength y)

    binRes res = intToWires (xyLength + 1) res
    badKeys :: (Int, Int) -> Set String
    badKeys (v1, v2) = S.fromList . badOutputs (binRes $ v1+v2) $ applyOps (valuesFrom v1 v2, opMap)

-- part2 :: Input -> [(String, Set String)]
-- part2 (defaultV, opMap) = intercalate "," . sort .  head $ filter (\l ->  (== res) . wiresToInt "z" $ applyOps (defaultV, swapThem opMap l)) $ keysToSwap -- (\e -> trace (show l) e).
part2 (defaultV, opMap) = intercalate "," . sort .  head $ filter (\l -> S.null . (\e -> trace (show (intercalate "," $l, e)) e) $  findInvalidGates (defaultV, swapThem opMap l)) $ keysToSwap --
                                --map (\l -> )) keysToSwap

  where
    defaultX = wiresToInt "x" defaultV
    defaultY = wiresToInt "y" defaultV
    res =  defaultX + defaultY
    binRes = intToWires (length . filter (isPrefixOf "x") $ M.keys defaultV) res
    opKeys =  M.keys opMap
    badKeys = (\e -> trace (show (length e, e)) e) . badOutputs binRes $ applyOps (defaultV, opMap)
    badDependencyMap = M.fromList $ map (id &&& (dependenciesOf opMap)) badKeys
    -- badDependencies = S.unions $ M.elems badDependencyMap
    keysToSwap = [["z06", "vwr", "tqm", "z11", "kfs", "z16", "gfv", "hcm"]] -- map ((++["z06", "vwr", "tqm", "z11", "kfs", "z16"]) . (\(a,b) -> [a,b])) $ zip (M.keys opMap) (tail $ M.keys opMap)       -- $ (\l -> map(\(x, y) -> [x,y]). zip l $ tail l) $ S.toList $ dependenciesOf' opMap 5 "z36"
    --  filter (\(i, l) ->  (all (\k -> not . S.null $ S.intersection (S.fromList l) (badDependencyMap M.! k)) badKeys)) .

    dependsOn mp key curr
      | curr `M.notMember` mp = False
      | (key `elem`) $ filter (`M.member` mp) [curr, lKey, rKey] = True
      | otherwise = any (dependsOn mp key) [lKey, rKey]
        where
          (Operation lKey rKey _) = mp M.! curr



    swapThem opMap [] = opMap
    swapThem opMap (k1:k2:l)
      | dependsOn opMap k1 k2 || dependsOn opMap k2 k1 = opMap
      | otherwise = swapThem newOpMap l
      where
        v1 = opMap M.! k1
        v2 = opMap M.! k2
        newOpMap = M.insert k1 v2 $ M.insert k2 v1 opMap

dependenciesOf :: Map String Operation -> String -> Set String
dependenciesOf mp key
  | key `M.notMember` mp = S.empty
  | otherwise = S.insert key . S.unions $ map (dependenciesOf mp) [lKey, rKey]
    where
      (Operation lKey rKey _) = mp M.! key

dependenciesOf' :: Map String Operation -> Int -> String -> Set String
dependenciesOf' mp d key
  | key `M.notMember` mp = S.empty
  | d == 0 = S.singleton key
  | otherwise = S.insert key . S.unions $ map (dependenciesOf' mp (d-1)) [lKey, rKey]
    where
      (Operation lKey rKey _) = mp M.! key

operations :: Map String Operation -> [String]
operations ops = map (\e -> e ++ " = " ++ opeOf 5 e) . sort $ filter ("z" `isPrefixOf`) $ M.keys ops
  where
    opeOf 0 e = e
    opeOf d e
      | "x" `isPrefixOf` e = e
      | "y" `isPrefixOf` e = e
      | otherwise = "(" ++ opeOf (d-1) l ++ ")" ++ "[" ++ l ++ " " ++ (show op) ++ " " ++ r ++ "]" ++ "(" ++ opeOf (d-1) r ++ ")"
        where
          (Operation l r op) = ops M.! e

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  -- print input

  -- print $intToWires 16 33
  print $ findInvalidGates input

  -- print $ (== 50411513338638) $ part1 input
  -- print $part1 input
  print $ part2 input
  -- putStrLn $ unlines $ operations $ snd input

  -- let mp = snd input
  -- let pairs l = zip l $ tail l
  -- print $ length $  pairs $ M.keys mp
  -- print $ dependenciesOf' mp 3 "z36"
  -- putStrLn $ unlines $ map show $ map (id &&& (dependenciesOf' mp 3)) . sort $ filter ("z" `isPrefixOf`) $ M.keys mp
