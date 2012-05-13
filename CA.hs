{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module CA where

import Data.Bits
import Data.Char
import Data.List
import Control.Concurrent
import System.Console.ANSI

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

class MultiShow a where
    multiShow :: a -> [Char]

padTranspose :: [[Char]] -> [String]
padTranspose [] = []
padTranspose xss = transpose . map pad $ xss where
    pad s = s ++ replicate (lines - length s) ' '
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

step :: Eq a => Automaton a -> [a] -> ([a], Int)
step Automaton { q_0 = q_0, delta = delta } qs =
    let qsPadded = [q_0,q_0] ++ qs ++ [q_0,q_0] in
    let qs' = map (\[q0,q1,q2] -> delta q0 q1 q2) . windowed 3 $ qsPadded in
    let pad = (length . takeWhile (== q_0) $ qs') - 1 in
    (reverse . dropWhile (== q_0) . reverse . dropWhile (== q_0) $ qs', pad)

run :: (Eq a, MultiShow a) => Automaton a -> [a] -> Int -> IO ()
run a tape padding = loop a (printTape padding tape) tape padding where
    loop a lastOut tape padding = do
        let out = printTape padding tape
        sequence_ $ zipWith diffOut lastOut out
        threadDelay 300000
        let (tape',padding') = step a tape
        loop a out tape' (padding + padding')
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


data StackCmd = Nop | Pop | Push Char deriving (Eq)

parseCmd :: Char -> StackCmd
parseCmd ' ' = Nop
parseCmd 'p' = Pop
parseCmd c   = Push c


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
