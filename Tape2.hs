{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Tape2 (tape2) where

import CA
import Data.Char

tape2 cmds = runStack (Automaton {
    q_0 = (' ',' '),
    delta = delta
    }) execCmd cmds
    where
        execCmd Pop _ _ = (' ',' ')
        execCmd (Push a) (b1,_) _ = (a,b1)
        execCmd _ (' ',' ') (c1,_) = (c1,' ') -- pop dest
        execCmd _ (b1,_) _ = (b1,' ')

        delta0 _ (b1,b2) _ = (b1,' ') -- push source

        delta1 (' ',' ') (b1,_) _ = (' ',' ') -- pop source
        delta1 _ (' ',' ') (c1,_) = (c1,' ') -- pop dest
        delta1 _ q1 _ = q1

        delta2 (a1,a2) (b1,_) _ | a2 /= ' ' = (a2,b1) -- push dest
        delta2 _ q1 _ = q1

        delta = delta2 `composeDelta` delta1 `composeDelta` delta0
