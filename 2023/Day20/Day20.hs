module Main where

import Data.Char
import Data.Maybe ( fromJust, isJust )
import Debug.Trace(trace)
import Data.Tuple
import Data.Tuple.Extra
import Data.List.Split
import Data.Set (Set, member, fromList, toList)
import Data.Map (Map, (!))
import qualified Data.Set as S
import qualified Data.Map as M

data AocModule = BroadCaster {name :: String, dst :: [String]} | Conjunction {name :: String, statuses :: Map String Bool, dst :: [String], hasRX::Bool} | FlipFlop {name :: String, state :: Bool, dst :: [String]} deriving (Show, Eq)

type Input = Map String AocModule

parseInput :: String -> Input
parseInput str = fixConjunctions modules [c | (c@(Conjunction {}))<- (M.elems modules)]
    where
        parseModule ('%':m) = (name, FlipFlop name False (splitOn ", " dsts))
            where
                [name, dsts] = splitOn " -> " m
        parseModule ('&':m) = (name, Conjunction name (M.empty) dsts ("rx" `elem` dsts))
            where
                [name, dstStr] = splitOn " -> " m
                dsts = splitOn ", " dstStr
        parseModule m = (name, BroadCaster name (splitOn ", " dsts))
            where
                [name, dsts] = splitOn " -> " m
        modules = M.fromList . map parseModule . lines $ str
        fixConjunctions :: Map String AocModule -> [AocModule] -> Map String AocModule
        fixConjunctions modules' [] = modules'
        fixConjunctions modules' ((Conjunction n statuses dsts hasRx):l) = fixConjunctions (M.insert n newC modules') l
            where
                sources = map name $ filter ((n `elem`) . dst) (M.elems modules')
                newC = Conjunction n (foldr (\s m -> M.insert s False m) statuses sources) dsts hasRx

add :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
add (x, y) (x2, y2) = (x+x2, y+y2)

incre :: Num b => (b, b) -> b -> Bool -> (b, b)
incre (x, y) nb False = (x+nb, y)
incre (x, y) nb True = (x, y+nb)


sendPulse :: Map String AocModule -> [(AocModule, Bool, String)] -> (Map String AocModule, (Int, Int))
sendPulse modules [] = (modules, (0, 0))
sendPulse modules ((BroadCaster name dsts, pulse, _):l) = (modules', incre sent (length dsts) pulse)
    where
        toSend = (map (\d -> (d, pulse, name)) . map (modules !) $ filter (`M.member` modules) dsts)
        (modules', sent) = sendPulse modules $ l ++ toSend
sendPulse modules ((FlipFlop name state dsts, pulse, _):l)
    | pulse     = (sendPulse modules l)
    | otherwise =  (modules', incre sent (length dsts) (not state))
        where
            toSend = (map (\d -> (d, not state, name)) . map (modules !) $ filter (`M.member` modules) dsts)
            (modules', sent) = sendPulse (M.insert name (FlipFlop name (not state) dsts) modules) $ l ++ toSend
sendPulse modules ((Conjunction name statuses dsts hasRx, pulse, src):l) =(modules', incre sent (length dsts) p)
    where
        p = not $ all (id) (M.elems newStatuses)
        newStatuses = M.insert src pulse statuses
        toSend = (map (\d -> (d, p, name)) . map (modules !) $ filter (`M.member` modules) dsts)
        (modules', sent) = sendPulse (M.insert name (Conjunction name newStatuses dsts hasRx) modules) $ l ++ toSend



part1 :: Input -> Int
part1 inp = uncurry (*) . (\e -> trace (show e) e). snd$ foldr sendPulses (inp, (0, 0)) [1..1000]
    where
        sendPulses _ (i, r) = (i', incre (add r r') 1 False)
            where (i', r') = sendPulse i [(i ! "broadcaster", False, "broadcaster")]

sendPulse2 :: Map String AocModule -> [String] -> [(AocModule, Bool, String)] -> (Map String AocModule, Set String)
sendPulse2 modules _ [] = (modules, S.empty)
sendPulse2 modules c ((BroadCaster name dsts, pulse, _):l) = sendPulse2 modules c $ l ++ toSend
    where
        toSend = (map (\d -> (d, pulse, name)) . map (modules !) $ filter (`M.member` modules) dsts)
sendPulse2 modules c ((FlipFlop name state dsts, pulse, _):l)
    | pulse     = sendPulse2 modules c l
    | otherwise = sendPulse2 (M.insert name (FlipFlop name (not state) dsts) modules) c $ l ++ toSend
        where
            toSend = (map (\d -> (d, not state, name)) . map (modules !) $ filter (`M.member` modules) dsts)
sendPulse2 modules c (te@(Conjunction name statuses dsts hasRx, pulse, src):l) = r
    where
        p = not $ all (id) (M.elems newStatuses)
        newStatuses = M.insert src pulse statuses
        toSend = (map (\d -> (d, p, name)) . map (modules !) $ filter (`M.member` modules) dsts)

        res = S.fromList . map fst . filter (snd) $ M.toList newStatuses
        updH = if not hasRx then S.empty else (res)

        (mds, updated) = sendPulse2 (M.insert name (Conjunction name newStatuses dsts hasRx) modules) c $ l ++ toSend
        r = (mds, S.union updated updH)

part2 :: Input -> Int
part2 inp = (recu inp 1 rxConjuncs)
    where
        recu i acc rxConjuncs
            | all (/= 0) (M.elems rxConjuncs) = trace (show rxConjuncs ++ " " ++ show (i ! "rs")) (foldl1 lcm $ M.elems rxConjuncs)
            | acc > 4200 = 42
            | otherwise = (recu i' (acc+1) newRxConjuncs)
            where
                (i', toUpd) = sendPulse2 i (M.keys rxConjuncs) [(i ! "broadcaster", False, "broadcaster")]
                newRxConjuncs = (foldr (\c rxcs -> trace ("Updating: " ++ c ++ " at : " ++ show acc) (M.insert c acc rxcs)) rxConjuncs $ toUpd)


        conjuncsOf n = [name c | (c@(Conjunction {}))<- (M.elems inp), n `elem` (dst c)]

        rxc = head $ conjuncsOf "rx"
        rxConjuncs :: Map String Int
        rxConjuncs = M.fromList $ zip (conjuncsOf (head $ conjuncsOf "rx")) (cycle [0])

main :: IO ()
main = do
    content <- readFile "input.txt"
    let inp = parseInput content
    -- print inp
    print $ part1 inp
    print $ part2 inp
