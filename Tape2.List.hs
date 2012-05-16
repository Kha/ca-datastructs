{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Tape2 (tape2) where

import CA
import Data.Char

instance MultiShow [Char] where
    multiShow = pad 2

tape2 cmds = runStack (Automaton {
    q_0 = [],
    delta = delta
    }) execCmd cmds
    where
        execCmd Pop _ _ = []
        execCmd (Push a) bs _ = a:one bs
        execCmd _ [] cs = one cs -- pop dest
        execCmd _ bs _ = one bs -- push source

        delta0 _ bs _ = one bs -- push source

        delta1 [] _ _ = [] -- pop source
        delta1 _ [] cs = one cs -- pop dest
        delta1 _ q1 _ = q1

        delta2 [a1,a2] bs _ = a2:one bs -- push dest
        delta2 _ q1 _ = q1

        one = take 1

        delta = delta2 `composeDelta` delta1 `composeDelta` delta0
