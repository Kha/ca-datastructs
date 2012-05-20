{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Tape2 (stack2) where

import CA
import Data.Char

stack2 cmds = runStack1 (Automaton {
    q_0 = (' ',' '),
    delta = delta
    }) liftCmd cmds
    where
        liftCmd Pop = (' ',' ')
        liftCmd (Push a) = ('x',a)
        liftCmd Nop = ('x',' ')

        delta0 _ (b1,b2) _ = (b1,' ') -- push source

        delta1 (' ',' ') (b1,_) _ = (' ',' ') -- pop source
        delta1 _ (' ',' ') (c1,_) = (c1,' ') -- pop dest
        delta1 _ q1 _ = q1

        delta2 (a1,a2) (b1,_) _ | a2 /= ' ' = (a2,b1) -- push dest
        delta2 _ q1 _ = q1

        delta = delta2 `composeDelta` delta1 `composeDelta` delta0

queue2 cmds = runStack1 (Automaton {
    q_0 = (' ',' '),
    delta = delta
    }) liftCmd cmds
    where
        liftCmd Pop = (' ',' ')
        liftCmd (Push a) = ('x',a)
        liftCmd Nop = ('x',' ')

        delta0 _ (' ',b) (a,_) = (a,b) -- pop dest
        delta0 (' ',_) (_,b) _ = (' ',b) -- pop source
        delta0 _ q1 _ = q1

        delta1 (_,b) (' ',_) (' ',_) = (b,' ') -- push source,dest
        delta1 (_,b) (a,_) _ = (a,b)

        delta = delta1 `composeDelta` delta0
