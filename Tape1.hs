module Tape1 (tape1) where

import CA
import Data.Char

data Tape1 =
    Cmd StackCmd
    | Empty
    | Cell (Char, StackCmd)
    deriving (Eq)

instance Show Tape1 where
    show (Cmd Nop) = " "
    show (Cmd Pop) = "p"
    show (Cmd (Push c)) = [c]
    show Empty = " "
    show (Cell (c, Nop)) = [c]
    show (Cell (c, _)) = [toUpper c]

instance MultiShow Tape1 where multiShow x = [show x]

tape1 cmds = run (Automaton {
    q_0 = Empty,
    delta = delta
    }) (map (Cmd . parseCmd) cmds ++ map (\c -> Cell (c, Nop)) "abcdefghijklmn") 20
    where
        delta cmd@(Cmd _) (Cmd _) _ = cmd
        delta Empty (Cmd _) _ = Empty
        delta (Cmd Pop) (Cell _) (Cell (c,_)) = Cell (c, Pop)
        delta (Cell (_, Pop)) (Cell _) (Cell (c,_)) = Cell (c, Pop)
        delta (Cell (_, Pop)) (Cell _) Empty = Empty
        delta (Cmd (Push c')) (Cell (c,_)) _ = Cell (c', Push c)
        delta (Cell (_, Push c')) (Cell (c,_)) _ = Cell (c', Push c)
        delta (Cell (_, Push c')) Empty _ = Cell (c', Nop)
        delta _ (Cell (c,_)) _ = Cell (c, Nop)
        delta _ q1 _ = q1
