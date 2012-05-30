{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Tape1 (stack1) where

import CA
import Data.Char

data Dir = L | N | R | R' deriving (Eq, Show)

instance MultiShow (Char, Dir) where
    multiShow (_,L) = ["<"]
    multiShow (c,N) = [[c]]
    multiShow (c,_) = [[toUpper c]]

stack1 cmds = runStack (Automaton {
    q_0 = (' ',N),
    delta = delta
    }) execCmd cmds
    where
        execCmd Pop _ _ = (' ',L)
        execCmd _ (_,L) _ = (' ',N)

        execCmd (Push a) _ _ = (a,R)
        execCmd _ (b,R) (_,_) = (b,R')
        execCmd _ (_,R') (_,N) = (' ',N)

        execCmd _ q1 _ = q1

        delta (a,R) (' ',N) _ = (a,N)
        delta (_,R) (b,N) _ = (b,R)
        delta _ (b,R) _ = (b,R')
        delta (a,_) (_,R') (_,N) = (a,N)

        delta (_,L) (' ',N) _ = (' ',N)
        delta (_,L) _ (c,_)   = (c,L)
        delta _ (b,L) _       = (b,N)

        delta _ q1 _ = q1
