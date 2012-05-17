{-# LANGUAGE FlexibleInstances #-}

module Tape3 (tape3) where

import CA

instance MultiShow [Char] where
    multiShow = pad 3

tape3 cmds = runStack (Automaton {
    q_0 = [],
    delta = delta
    }) execCmd cmds
    where
        execCmd Pop [b] cs = one cs -- pop source,dest
        execCmd Pop bs _ = drop 1 $ two bs -- pop source
        execCmd (Push a) [b] cs = a:b:one cs -- push dest, pop dest
        execCmd (Push a) bs _ = a:two bs -- push dest
        execCmd _ [b] cs = b:one cs -- pop dest
        execCmd _ bs _ = two bs -- push source

        delta0 as bs _ = two bs -- push source

        delta1 as bs _ | length as <= 1 = drop 1 bs -- pop source
        delta1 _ q1 _ = q1

        delta2 _ bs cs | length bs <= 1 = bs++one cs -- pop dest
        delta2 _ q1 _ = q1

        delta3 [_,_,a3] bs _ = a3:two bs -- push dest
        delta3 _ q1 _ = q1

        one = take 1
        two = take 2

        delta = delta3 `composeDelta` delta2 `composeDelta` delta1 `composeDelta` delta0
