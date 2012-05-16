{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TupleSections #-}

module CA where

import Data.Bits
import Data.Char
import Data.List
import Control.Concurrent
import System.Console.ANSI
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

data Automaton a = Automaton {
    q_0 :: a,
    delta :: Delta a
}

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


class MultiShow a where
    multiShow :: a -> [Char]

instance Show a => MultiShow a where
    multiShow a = show a

instance MultiShow Char where
    multiShow c = [c]

pad :: Int -> String -> String
pad n s = s ++ replicate (n - length s) ' '

padTranspose :: [[Char]] -> [String]
padTranspose [] = []
padTranspose xss = transpose . map (pad lines) $ xss where
    lines = foldr1 max . map length $ xss

bracketizeLines :: [String] -> [String]
bracketizeLines lines = zipWith line [0..] lines where
    line 0 l = "⎡" ++ l ++ "⎤"
    line n l | n == length lines - 1 = "⎣" ++ l ++ "⎦"
    line _ l = "⎢" ++ l ++ "⎥"

windowed :: Int -> [a] -> [[a]]
windowed size xs@(x:xs') = case take size xs of
    win | length win == size -> win : windowed size xs'
    _ -> []

diffOut :: String -> String -> IO ()
diffOut x y = loop $ zip (x ++ repeat ' ') y where
    loop [] = putStrLn ""
    loop xs = do
        putStr . map snd $ same
        setSGR [SetColor Foreground Vivid Red]
        putStr . map snd $ unsame
        setSGR [SetColor Foreground Vivid White]
        loop rest'
        where
            (same, rest) = span (uncurry (==)) xs
            (unsame, rest') = span (uncurry (/=)) rest

step :: Eq a => Automaton a -> [a] -> Maybe ([a], Int)
step Automaton { q_0 = q_0, delta = delta } qs =
    let qsPadded = [q_0,q_0] ++ qs ++ [q_0,q_0] in
    let qs' = map (\[q0,q1,q2] -> delta q0 q1 q2) . windowed 3 $ qsPadded in
    let pad = (length . takeWhile (== q_0) $ qs') - 1 in
    let qs'Unpadded = reverse . dropWhile (== q_0) . reverse . dropWhile (== q_0) $ qs' in
    if qs'Unpadded == qs && pad == 0
       then Nothing
       else Just (qs'Unpadded, pad)

run :: (Eq a, MultiShow a) => Automaton a -> [a] -> Int -> IO ()
run a tape padding = do
    n <- loop a (printTape padding tape) tape padding 1
    putStrLn $ "Halted after " ++ show n ++ " steps"
    where
    loop a lastOut tape padding n = do
        let out = printTape padding tape
        sequence_ $ zipWith diffOut lastOut out
        threadDelay 300000
        case step a tape of
            Just (tape',padding') -> loop a out tape' (padding + padding') (n+1)
            Nothing -> return n

    printTape padding = map (take 70 . pad padding) . bracketizeLines . padTranspose . map multiShow
    pad n = drop (-n) . (replicate n ' ' ++)


-- 110


instance MultiShow Int where
    multiShow 0 = " "
    multiShow x = show x

rule :: Int -> Automaton Int
rule n = Automaton {
        q_0 = 0,
        delta = delta
    } where
        delta q0 q1 q2 = if testBit n (q0*4+q1*2+q2) then 1 else 0

turing = run (rule 110) [1] 60


-- generic stack


instance (MultiShow a, MultiShow b) => MultiShow (Either a b) where
    multiShow (Left a) = multiShow a
    multiShow (Right b) = multiShow b

shiftAutomaton q_0 = Automaton {
        delta = delta,
        q_0 = q_0
    } where
        delta q0 _ _ = q0

data StackCmd = Nop | Pop | Push Char deriving (Eq)

instance MultiShow StackCmd where
    multiShow Nop = " "
    multiShow Pop = "<"
    multiShow (Push c) = [c]

parseCmd :: Char -> StackCmd
parseCmd ' ' = Nop
parseCmd '<' = Pop
parseCmd c   = Push c

runStack :: (Eq b, MultiShow b) => Automaton b -> (StackCmd -> b -> b -> b) -> [Char] -> IO ()
runStack a execCmd cmds = run cmdA (cmds' ++ [Right (q_0 a)]) 1 where
    cmdA = concatAutomata (shiftAutomaton Nop) a transLeft execCmd
    cmds' = map (Left . parseCmd) cmds
    transLeft q0 _ _ = q0


-- reverse


instance MultiShow (Maybe Char) where
    multiShow (Just c) = [c]
    multiShow Nothing  = " "

instance (MultiShow a, MultiShow b) => MultiShow (a,b) where
    multiShow (x,y) = multiShow x ++ multiShow y

reverseString s = run (Automaton {
    q_0 = (Nothing,Nothing),
    delta = delta
    }) (map ((, Just ' ') . Just) s) 20
    where
        deltaTop :: Delta (Maybe Char)
        deltaTop _ (Just ' ') (Just c) = Just c
        deltaTop (Just ' ') (Just _) _ = Just ' '
        deltaTop _ q0 _ = q0

        deltaCrossover :: Delta (Maybe Char, Maybe Char)
        deltaCrossover (Nothing,Nothing) (Just c,Just ' ') _ = (Just ' ',Just c)
        deltaCrossover _ q0 _ = q0

        delta = deltaCrossover `eitherDelta` (deltaTop `cartesianDelta` reverseDelta deltaTop)
