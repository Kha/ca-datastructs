{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module CA.StackLike where

import CA
import CA.Output
import Data.List
import Data.Monoid
import System.IO
import Control.Concurrent
import Control.Monad

data StackCmd s = Nop | Pop | Push s deriving (Eq)

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

delta' :: (Eq q) => StackLike q s -> StackCmd s -> [q] -> [q]
delta' stack cmd = stepNatural (aut stack) (cell1 stack cmd)

endoPower :: (a -> a) -> Int -> (a -> a)
endoPower f n = appEndo . mconcat . replicate n . Endo $ f

deltaStar :: (Eq q) => StackLike q s -> StackCmd s -> [q] -> [q]
deltaStar stack Nop = delta' stack Nop
deltaStar stack (Push a) = delta' stack (Push a) . endoPower (delta' stack Nop) (pushBubble stack)
deltaStar stack Pop = delta' stack Pop . endoPower (delta' stack Nop) (popBubble stack)

pushMany :: (Eq q) => StackLike q s -> [s] -> [q] -> [q]
pushMany stack = appEndo . mconcat . map (Endo . deltaStar stack . Push) . reverse

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
    loop False tape (printTape 10 tape) where
    loop pause tape lastOut = do
        let out = printTape 0 tape
        when (not pause) $ sequence_ $ zipWith diffOut lastOut out
        input <- (\ready -> ready || pause) `fmap` hReady stdin
        cmd <- if not input then return Nop else
            (\a -> if a == 'x' then Pop else Push a) `fmap` getChar
        threadDelay 600000
        case delta' stack cmd tape of
            tape' | tape' == tape -> loop True tape' out
            tape' -> loop False tape' out
