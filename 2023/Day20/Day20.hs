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
import qualified Data.List as L

data AocModule = BroadCaster {name :: String, dst :: [String]}                                           |
                 Conjunction {name :: String, statuses :: Map String Bool, dst :: [String]} |
                 FlipFlop {name :: String, state :: Bool, dst :: [String]} deriving (Eq)

type Input = Map String AocModule

instance Show AocModule where
    show BroadCaster {} = "BroadCaster"
    show (Conjunction _ s _) = "Conjunction{"++show (M.toAscList s)++ "}"
    show (FlipFlop _ s d) = "FlipFlop{"++ show s ++ ","++ show d ++ "}"


parseInput :: String -> Input
parseInput str = fixConjunctions modules [c | (c@(Conjunction {}))<- (M.elems modules)]
    where
        parseModule m
            | head m == '%' = (name, FlipFlop name False dsts)
            | head m == '&' = (name, Conjunction name (M.empty) dsts)
            | otherwise     = (name, BroadCaster name dsts)
            where
                name = head splitted
                dstStr = if length splitted == 2 then splitted !! 1 else ""
                splitted = splitOn " -> " $ dropWhile (not . isAlpha) m
                dsts = if length dstStr == 0 then [] else splitOn ", " dstStr
        modules = M.fromList . map parseModule . lines $ str
        fixConjunctions :: Map String AocModule -> [AocModule] -> Map String AocModule
        fixConjunctions modules' [] = modules'
        fixConjunctions modules' ((Conjunction n statuses dsts):l) = fixConjunctions (M.insert n newC modules') l
            where
                sources = map name $ filter ((n `elem`) . dst) (M.elems modules')
                newC = Conjunction n (foldr (\s m -> M.insert s False m) statuses sources) dsts

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
    | otherwise = (modules', incre sent (length dsts) (not state))
        where
            toSend = (map (\d -> (d, not state, name)) . map (modules !) $ filter (`M.member` modules) dsts)
            (modules', sent) = sendPulse (M.insert name (FlipFlop name (not state) dsts) modules) $ l ++ toSend
sendPulse modules ((Conjunction name statuses dsts, pulse, src):l) = (modules', incre sent (length dsts) p)
    where
        p = not $ all (id) (M.elems newStatuses)
        newStatuses = M.insert src pulse statuses
        toSend = (map (\d -> (d, p, name)) . map (modules !) $ filter (`M.member` modules) dsts)
        (modules', sent) = sendPulse (M.insert name (Conjunction name newStatuses dsts) modules) $ l ++ toSend

part1 :: Input -> Int
part1 inp = uncurry (*) . snd$ foldr sendPulses (inp, (0, 0)) [1..1000]
    where
        sendPulses _ (i, r) = (i', incre (add r r') 1 False)
            where (i', r') = sendPulse i [(i ! "broadcaster", False, "broadcaster")]

isConjunction :: AocModule -> Bool
isConjunction (Conjunction {}) = True
isConjunction _                = False

isFlipFlop :: AocModule -> Bool
isFlipFlop (FlipFlop {}) = True
isFlipFlop _                = False


sendPulse2 :: Int -> Map String AocModule -> [(String, Bool, String)] -> (Map String AocModule, Set String)
sendPulse2 n modules [] = (modules, S.empty)
sendPulse2 n modules [("broadcaster", pulse, _)] = sendPulse2 n modules toSend
    where
        modle = modules ! "broadcaster"
        toSend = (map (\d -> (d, pulse, name modle)) . filter (`M.member` modules)$ dst modle)
sendPulse2 n modules ((curr, pulse, src):l)
    | isFlipFlop modle = handleFlipFlop modle
    | isConjunction modle = handleConj modle
        where
            modle = modules ! curr
            handleFlipFlop (FlipFlop name state dsts)
                | pulse     = sendPulse2 n modules l
                | otherwise = sendPulse2 n (M.insert name (FlipFlop name (not state) dsts) modules) $ l ++ toSend
                    where
                        toSend = (map (\d -> (d, not state, name)) $ filter (`M.member` modules) dsts)

            handleConj (conj@(Conjunction na sts dsts)) = (modules', updated')
                where
                    newSts = M.insert src pulse sts
                    p = not $ all id (M.elems newSts)
                    toSend = map (\m -> (m, p, na)) $ filter (`M.member` modules) dsts

                    (modules', updated)= sendPulse2 n (M.insert na (conj {statuses=newSts}) modules) (l++toSend)

                    updated' = if pulse /= True || "rx" `notElem` dsts then updated else (S.insert src updated)

part2 :: Input -> Int
part2 inp = recu 1 (inp, M.fromList $ zip mustBeHigh $ cycle [0])
    where
        recu :: Int -> (Map String AocModule, Map String Int) -> Int
        recu acc (i, rxConjuncs)
            | all (/= 0) (M.elems rxConjuncs) =  foldr1 lcm $ M.elems rxConjuncs
            | acc > 4200 = 42
            | otherwise = (recu (acc+1) (i', rxc'))
            where
                (i', toUpdate) = sendPulse2 acc i [("broadcaster", False, "button")]
                rxc' = foldl (\r d -> M.insert d acc r) rxConjuncs toUpdate

        mustBeHigh = M.keys . statuses . fromJust . L.find (("rx" `elem`) . dst) . M.elems $ inp


-- This solution is not working while the other is, why ? no idea
-- The only difference between the two is the first one take the name of the module while the other one the module itself
{-
sendPulse2 :: Int -> Map String AocModule -> [(AocModule, Bool, String)] -> (Map String AocModule, Set String)
sendPulse2 n modules [] = (modules, S.empty)
sendPulse2 n modules [(BroadCaster na dsts, pulse, _)] = sendPulse2 n modules toSend
    where
        toSend = map (\d -> (d, pulse, na)) . map (modules !) . filter (`M.member` modules)$ dsts
sendPulse2 n modules ((modle, pulse, src):l)
    | isFlipFlop modle = handleFlipFlop modle
    | isConjunction modle = handleConj modle
        where
            handleFlipFlop (FlipFlop name state dsts)
                | pulse     = sendPulse2 n modules l
                | otherwise = sendPulse2 n (M.insert name (FlipFlop name (not state) dsts) modules) $ l ++ toSend
                    where
                        toSend = (map (\d -> (d, not state, name)) . map (modules !) . filter (`M.member` modules) $ dsts)

            handleConj (conj@(Conjunction na sts dsts)) = (modules', updated')
                where
                    newSts = M.insert src pulse sts
                    p = not $ all id (M.elems newSts)
                    toSend = map (\m -> (m, p, na)) . map (modules !) . filter (`M.member` modules)$ dsts

                    (modules', updated)= sendPulse2 n (M.insert na (conj {statuses=newSts}) modules) (l++toSend)

                    updated' = if pulse /= True || "rx" `notElem` dsts then updated else (S.insert src updated)

part2 :: Input -> Int
part2 inp = recu 1 (inp, M.fromList $ zip mustBeHigh $ cycle [0])
    where
        recu :: Int -> (Map String AocModule, Map String Int) -> Int
        recu acc (i, rxConjuncs)
            | all (/= 0) (M.elems rxConjuncs) = foldr1 lcm $ M.elems rxConjuncs
            | acc > 4200 = 42
            | otherwise = (recu (acc+1) (i', rxc'))
            where
                (i', toUpdate) = sendPulse2 acc i [(i!"broadcaster", False, "button")]
                rxc' = foldl (\r d -> M.insert d acc r) rxConjuncs toUpdate

        mustBeHigh = M.keys . statuses . fromJust . L.find (("rx" `elem`) . dst) . M.elems $ inp

-}



main :: IO ()
main = do
    content <- readFile "input.txt"
    let inp = parseInput content
    -- print inp
  print $ part1 inp
    print $ part2 inp
