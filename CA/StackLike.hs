{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module CA.StackLike where

import CA
import CA.Output
import Data.List

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
    gamma :: q -> Maybe s
}

fromAutomaton :: Automaton q -> (StackCmd s -> q) -> (q -> Maybe s) -> StackLike q s
fromAutomaton aut liftCmd gamma = StackLike {
    aut = aut,
    cell1 = (delta aut) . liftCmd,
    gamma = gamma
}

shiftAutomaton q_0 = Automaton {
        delta = delta,
        q_0 = q_0
    } where
        delta q0 _ _ = q0

-- test a stack-like data structure by putting 'cmds' on the left
-- side of the tape and feeding the stack-like one by one
testStackLike :: (Eq q, Eq s) => StackLike q s -> [StackCmd s] -> Configuration (Either (StackCmd s) q)
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
