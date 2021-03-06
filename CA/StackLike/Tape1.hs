{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module CA.StackLike.Tape1 (stack1) where

import CA
import CA.Output
import CA.StackLike
import Data.Char

data Dir = L Int | N | R deriving (Eq,Show)

instance MultiShow (Char,Dir) where
    multiShow (c,L 0) = ["<"]
    multiShow (c,L _) = ["«"]
    multiShow (c,N) = multiShow c
    multiShow (c,R) = [[toUpper c]]

stack1 :: StackLike (Char,Dir) Char
stack1 = (fromAutomaton Automaton {
        q_0 = (' ',N),
        delta = delta
    } liftCmd) { gamma = gamma, pushBubble = 1, popBubble = 2 }
    where
        liftCmd Pop = (' ',L 0)
        liftCmd Nop = (' ',R)
        liftCmd (Push c) = (c,R)

        -- doesn't work since cell 1 is usually empty...
        gamma (' ',_) = Nothing
        gamma (a,_)   = Just a

        delta0 _ (_,R) _ = (' ',N) -- push source
        delta0 _ q1 _ = q1

        delta1 (c,R) (' ',_) _ = (c,N) -- push dest
        delta1 (c1,d) (c2,N) _ | (c1 /= ' ' || d == R) && c2 /= ' ' = (c2,R) -- push source choose
        delta1 _ q1 _ = q1

        delta2 (_,L 2) (' ',_) (' ',_) = (' ',N)
        delta2 (_,L i) (' ',_) (' ',_) = (' ',L $ i+1)
        delta2 (_,L _) _ _ = (' ',L 0)
        delta2 _ (_,L _) (c,_) = (c,N)
        delta2 _ q1 _ = q1

        delta = delta2 `composeDelta` delta1 `composeDelta` delta0
