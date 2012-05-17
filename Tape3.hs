{-# LANGUAGE FlexibleInstances #-}

module Tape3 (tape3) where

import CA

instance MultiShow [Char] where
    multiShow = pad 3

tape3 cmds = runStack1 (Automaton {
    q_0 = [],
    delta = delta
    }) liftCmd cmds
    where
        liftCmd (Push a) = ['x','x',a]
        liftCmd Pop = []
        liftCmd Nop = ['x','x']

        delta0 as bs _ = two bs -- push source

        delta1 as [b] cs | length as <= 1 = one cs -- pop source, dest
        delta1 as bs _ | length as <= 1 = drop 1 bs -- pop source
        delta1 _ bs cs | length bs <= 1 = bs++one cs -- pop dest
        delta1 _ q1 _ = q1

        delta3 [_,_,a3] bs _ = a3:two bs -- push dest
        delta3 _ q1 _ = q1

        one = take 1
        two = take 2

        delta = delta3 `composeDelta` delta1 `composeDelta` delta0
