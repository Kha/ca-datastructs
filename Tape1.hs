{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Tape1 (tape1) where

import CA
import Data.Char

type Tape1 = (Char, StackCmd)

instance Show Tape1 where
    show (' ',Pop) = "p"
    show (' ',Push c) = [c]
    show (c, Nop) = [c]
    show (c, _) = [toUpper c]

instance MultiShow Tape1 where multiShow x = [show x]

tape1 cmds = run (Automaton {
    q_0 = (' ',Nop),
    delta = delta
    }) (map ((' ',) . parseCmd) cmds ++ map (, Nop) "abcdefghijklmn") 20
    where
        delta cmd@(' ',_) (' ',_) _ = cmd
        delta (_,Pop) _ (' ',_) = (' ',Nop)
        delta (_,Pop) _ (c,_) = (c,Pop)
        delta (_,Push c') (' ',_) _ = (c',Nop)
        delta (_,Push c') (c,_) _ = (c',Push c)
        delta _ (c,_) _ = (c,Nop)
