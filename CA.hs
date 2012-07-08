module CA where

import Data.Char
import Data.List
import Data.Maybe

type Delta a = a -> a -> a -> a

reverseDelta :: Delta a -> Delta a
reverseDelta d q0 q1 q2 = d q2 q1 q0

cartesianDelta :: Delta a -> Delta b -> Delta (a,b)
cartesianDelta d0 d1 (p0,q0) (p1,q1) (p2,q2) = (d0 p0 p1 p2, d1 q0 q1 q2)

composeDelta :: Delta a -> Delta a -> Delta a
composeDelta d0 d1 q0 q1 q2 = d0 q0 (d1 q0 q1 q2) q2

eitherDelta :: Eq a => Delta a -> Delta a -> Delta a
eitherDelta d0 d1 q0 q1 q2 =
    let q1' = d0 q0 q1 q2 in
    if q1' == q1 then d1 q0 q1 q2 else q1'

data Automaton q = Automaton {
    q_0 :: q,
    delta :: Delta q
}

type Configuration q = (Automaton q, [q])

concatAutomata :: (Eq a, Eq b) => Automaton a -> Automaton b -> (a -> a -> b -> a) -> (a -> b -> b -> b) -> Automaton (Either a b)
concatAutomata Automaton { q_0 = q0_0, delta = delta0 } Automaton { q_0 = q1_0, delta = delta1 } transLeft transRight = Automaton {
        q_0 = Left q0_0,
        delta = d
    } where
        d (Left q0) (Left q1) (Left q2) = Left $ delta0 q0 q1 q2
        d (Left q0) (Left q1) (Right q2) = Left $ transLeft q0 q1 q2
        d (Left q0) (Right q1) q2 = Right $ transRight q0 q1 (extr q2)
        d (Right q0) q1 (Right q2) = Right $ delta1 q0 (extr q1) q2
        d (Right q0) q1 q2 = case delta1 q0 (extr q1) (extr q2) of
                             q | q == q1_0 && d q1 q2 (Left q0_0) == Left q0_0 -> Left q0_0
                             q -> Right q

        extr (Left _) = q1_0
        extr (Right q) = q

concatAutomata1 :: (Eq b, Eq a) => Automaton a -> Automaton b -> (b -> a) -> (a -> b) -> Automaton (Either a b)
concatAutomata1 a0 a1 toLeft toRight = concatAutomata a0 a1 transLeft transRight where
    transLeft q0 q1 q2 = delta a0 q0 q1 (toLeft q2)
    transRight q0 q1 q2 = delta a1 (toRight q0) q1 q2

windowed :: Int -> [a] -> [[a]]
windowed size xs@(x:xs') = case take size xs of
    win | length win == size -> win : windowed size xs'
    _ -> []

pad :: Int -> a -> [a] -> [a]
pad n x xs = xs ++ replicate (n - length xs) x

step :: Eq a => Configuration a -> Maybe (Configuration a, Int)
step (a @ Automaton { q_0 = q_0, delta = delta },tape) =
    let tapePadded = [q_0,q_0] ++ tape ++ [q_0,q_0] in
    let tape' = map (\[q0,q1,q2] -> delta q0 q1 q2) . windowed 3 $ tapePadded in
    let pad = (length . takeWhile (== q_0) $ tape') - 1 in
    let tape'Unpadded = reverse . dropWhile (== q_0) . reverse . dropWhile (== q_0) $ tape' in
    if tape'Unpadded == tape && pad == 0
       then Nothing
       else Just ((a,tape'Unpadded), pad)

stepNatural :: Eq a => Configuration a -> (a -> a -> a) -> Maybe (Configuration a)
stepNatural (a @ Automaton { q_0 = q_0, delta = delta },tape) delta1 =
    let tapePadded = tape ++ [q_0,q_0] in
    let tape' = delta1 (tapePadded !! 0) (tapePadded !! 1) : (map (\[q0,q1,q2] -> delta q0 q1 q2) . windowed 3 $ tapePadded) in
    let tape'Unpadded = reverse . dropWhile (== q_0) . reverse $ tape' in
    if tape'Unpadded == tape
       then Nothing
       else Just (a,tape'Unpadded)
