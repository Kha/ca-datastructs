{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Tape2 (stack2) where

import CA
import Data.Char

instance MultiShow [Char] where
    multiShow = pad 2

tape2 cmds = runStack1 (Automaton {
    q_0 = [],
    delta = delta
    }) liftCmd cmds
    where
        liftCmd (Push a) = ['x',a]
        liftCmd Pop = []
        liftCmd Nop = ['x']

        delta0 _ bs _ = one bs -- push source

        delta1 [] _ _ = [] -- pop source
        delta1 _ [] cs = one cs -- pop dest
        delta1 _ q1 _ = q1

        delta2 [a1,a2] bs _ = a2:one bs -- push dest
        delta2 _ q1 _ = q1

        one = take 1

        delta = delta2 `composeDelta` delta1 `composeDelta` delta0
