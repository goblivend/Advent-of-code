module Main where

import Data.Bits
import Data.Function
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

type Input = Map String (Set String)

type Output = Int

parseInput :: String -> Input
parseInput input = foldl (\m (c1, c2) -> M.insertWith (S.union) c1 (S.singleton c2) $ M.insertWith S.union c2 (S.singleton c1) m) M.empty connections
  where
    connections = map (second (drop 1) . splitAt 2) $ lines input

part1 :: Input -> Output
part1 input = S.size . S.filter (not . S.null . S.filter (isPrefixOf "t")) $ S.fromList triples
  where
    triples = concat $ map (uncurry tmp) $ M.assocs input
    tmp k v = S.toList . S.unions . S.toList $ S.map (\c -> S.map (S.union (S.fromList [k, c])) . S.map S.singleton $ S.intersection v (input M.! c)) v

join :: [a] -> [[a]] -> [a]
join i [] = []
join i (e : l) = foldl (\r e -> r ++ i ++ e) e l

part2 :: Input -> String
part2 input = join "," . sort $ S.toList biggest
  where
    isLan s = {-# SCC isLan #-} all (\e -> S.isSubsetOf (S.delete e s) $ input M.! e) s
    lansOf k = {-# SCC lansOf #-} S.powerSet . S.insert k $ (input M.! k)
    lans = {-# SCC lans #-} S.filter isLan . S.unions . map lansOf $ M.keys input

    biggest = {-# SCC biggest #-} maximumBy (compare `on` S.size) lans

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  -- print input

  print $ part1 input -- (== 1154) $
  print $ part2 input -- (== "aj,ds,gg,id,im,jx,kq,nj,ql,qr,ua,yh,zn") $
