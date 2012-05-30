{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Tape1 (stack1) where

import CA
import Data.Char

data Dir = L1 | L2 | N | R deriving (Eq,Show)

instance MultiShow (Char,Dir) where
    multiShow (c,L1) = ["<"]
    multiShow (c,L2) = ["«"]
    multiShow (c,N) = multiShow c
    multiShow (c,R) = [[toUpper c]]

stack1 cmds = runStack1 (Automaton {
    q_0 = (' ',N),
    delta = delta
    }) liftCmd cmds
    where
        liftCmd Pop = (' ',L1)
        liftCmd Nop = (' ',R)
        liftCmd (Push c) = (c,R)

        delta0 _ (_,R) _ = (' ',N) -- push source
        delta0 _ q1 _ = q1

        delta1 (c,R) (' ',_) _ = (c,N) -- push dest
        delta1 (c1,d) (c2,N) _ | (c1 /= ' ' || d == R) && c2 /= ' ' = (c2,R) -- push source choose
        delta1 _ q1 _ = q1

        delta2 (_,L1) (' ',_) (' ',_) = (' ',N)
        delta2 (_,L1) (' ',_) (c,_) = (c,L1)
        delta2 (_,L1) _ (c,_) = (c,N)
        delta2 _ (c,L1) _ = (c,L2)
        delta2 _ (_,L2) (' ',_) = (' ',L1)
        delta2 _ (_,L2) (c,_) = (c,N)
        delta2 _ q1 _ = q1

        delta = delta2 `composeDelta` delta1 `composeDelta` delta0
