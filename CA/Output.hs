{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module CA.Output where

import CA
import Data.List
import Control.Concurrent
import System.Console.ANSI

padMany :: [String] -> [String]
padMany [] = []
padMany xss = map (pad lines ' ') $ xss where
    lines = foldr1 max . map length $ xss

padTranspose :: [[String]] -> [String]
padTranspose [] = []
padTranspose xss = map concat . transpose . map (padMany . pad lines " ") $ xss where
    lines = foldr1 max . map length $ xss

class MultiShow a where
    multiShow :: a -> [String]

instance Show a => MultiShow a where
    multiShow a = [show a]

instance MultiShow Char where
    multiShow c = [[c]]

class Tape a where
    tapeShow :: [a] -> [String]

instance MultiShow a => Tape a where
    tapeShow = padTranspose . map multiShow

bracketizeLines :: [String] -> [String]
bracketizeLines lines = zipWith line [0..] lines where
    line 0 l = "⎡" ++ l ++ "⎤"
    line n l | n == length lines - 1 = "⎣" ++ l ++ "⎦"
    line _ l = "⎢" ++ l ++ "⎥"

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

printTape :: (Tape a) => Int -> [a] -> [String]
printTape padding = map (take 70 . padString padding) . bracketizeLines . tapeShow where
    padString n = drop (-n) . (replicate n ' ' ++)

runWithOptions :: (Eq a) => Automaton a -> Bool -> (Int -> [a] -> [String]) -> [a] -> Int -> IO ()
runWithOptions a pause printTape tape padding = do
    n <- loop pause a (printTape padding tape) tape padding 1
    putStrLn $ "Halted after " ++ show n ++ " steps"
    where
        --loop :: (Eq b) => Automaton b -> [String] -> [b] -> Int -> Int -> IO Int
        loop pause a lastOut tape padding n = do
            let out = printTape padding tape
            sequence_ $ zipWith diffOut lastOut out
            pause <- if pause
                then getChar >>= return . (== ' ')
                else threadDelay 300000 >> return False
            case step a tape of
                Just (tape',padding') -> loop pause a out tape' (padding + padding') (n+1)
                Nothing -> return n

run :: (Eq a, Tape a) => Automaton a -> [a] -> Int -> IO ()
run a tape padding = runWithOptions a False printTape tape padding
