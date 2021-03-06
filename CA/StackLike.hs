{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TupleSections #-}

module CA.StackLike where

import CA
import CA.Output
import Data.List
import Data.Maybe
import Data.Monoid
import System.IO
import Control.Concurrent
import Control.Monad

data StackCmd s = Nop | Pop | Push s deriving (Eq, Show)

instance MultiShow (StackCmd Char) where
    multiShow Nop = [" "]
    multiShow Pop = ["<"]
    multiShow (Push c) = multiShow c

parseCmd :: Char -> StackCmd Char
parseCmd ' ' = Nop
parseCmd '<' = Pop
parseCmd c   = Push c

data StackLike q s = StackLike {
    aut :: Automaton q,
    cell1 :: StackCmd s -> q -> q -> q,
    gamma :: q -> Maybe s,
    pushBubble :: Int,
    popBubble :: Int
}

gamma' :: StackLike q s -> [q] -> Maybe s
gamma' stack [] = Nothing
gamma' stack (q:_) = gamma stack q

delta' :: (Eq q) => StackLike q s -> StackCmd s -> [q] -> [q]
delta' stack cmd = stepNatural (aut stack) (cell1 stack cmd)

endoPower :: (a -> a) -> Int -> (a -> a)
endoPower f n = appEndo . mconcat . replicate n . Endo $ f

deltaPushStar stack a = delta' stack (Push a) . endoPower (delta' stack Nop) (pushBubble stack)
deltaPopStar stack tape = (gamma' stack bubbled,delta' stack Pop $ bubbled) where
    bubbled = endoPower (delta' stack Nop) (popBubble stack) tape

pushMany :: (Eq q) => StackLike q s -> [s] -> [q] -> [q]
pushMany stack = appEndo . mconcat . map (Endo . deltaPushStar stack) . reverse

bigGamma stack tape = case deltaPopStar stack tape of
    (Nothing,_) -> []
    (Just a,tape') -> a : bigGamma stack tape'

fromAutomaton :: Automaton q -> (StackCmd s -> q) -> StackLike q s
fromAutomaton aut liftCmd = StackLike {
    aut = aut,
    cell1 = (delta aut) . liftCmd,
    gamma = undefined,
    pushBubble = 0,
    popBubble = 0
}

shiftAutomaton q_0 = Automaton {
        delta = delta,
        q_0 = q_0
    } where
        delta q0 _ _ = q0

-- test a stack-like data structure by putting 'cmds' on the left
-- side of the tape and feeding the stack-like one by one
testStackLike :: (Eq q, Eq s) => StackLike q s -> [StackCmd s] -> AutWithTape (Either (StackCmd s) q)
testStackLike stack cmds = (a,tape) where
    a = concatAutomata (shiftAutomaton Nop) (aut stack) cell0 (cell1 stack)
    cell0 q0 _ _ = q0
    tape = map Left cmds ++ [Right . q_0 . aut $ stack]

instance (MultiShow a, MultiShow b) => Tape (Either a b) where
    tapeShow = padTranspose . map (col . reverse) . inits where
        col (Right b:Left _:_) = map ("|"++) $ multiShow b
        col [Right b] = map ("|"++) $ multiShow b
        col (Left a:_) = multiShow a
        col (Right b:_) = multiShow b
        col [] = []

interactiveStackLike :: (Eq q, Tape q) => StackLike q Char -> [q] -> IO ()
interactiveStackLike stack tape = do
    hSetEcho stdin False
    loop False tape (printTape 0 tape) True where
    loop pause tape lastOut first = do
        input <- (\ready -> ready || pause) `fmap` hReady stdin
        cmd <- if not input then return Nop else
            (\a -> if a == 'x' then Pop else Push a) `fmap` getChar
        threadDelay 600000
        case delta' stack cmd tape of
            tape' | not first && tape' == tape -> loop True tape' lastOut False
            tape' -> do
                let out = printTape 0 tape'
                sequence_ . zipWith (>>) (cmdOut cmd tape) $ zipWith diffOut lastOut out
                loop False tape' out False

    cmdOut cmd tape = map putStr $ pad 12 ' ' (showCmd cmd tape) : repeat (replicate 12 ' ')
    showCmd Pop tape = show (Pop :: StackCmd Char) ++ " => " ++ show (fromMaybe ' ' . gamma stack $ head tape)
    showCmd cmd _ = show cmd
