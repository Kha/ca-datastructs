{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module CA.StackLike.Tape2 (stack2,queue2) where

import CA
import CA.Output
import CA.StackLike
import Data.Char
import Data.Maybe

instance MultiShow [Char] where
    multiShow = map (:[]) . pad 2 ' '

stack2 = (fromAutomaton Automaton {
        q_0 = [],
        delta = delta
    } liftCmd) { gamma = listToMaybe, popBubble = 1 }
    where
        liftCmd (Push a) = [undefined,a]
        liftCmd Pop = []
        liftCmd Nop = [undefined]

        delta0 _ bs _ = one bs -- push source

        delta1 [] _ _ = [] -- pop source
        delta1 _ [] cs = one cs -- pop dest
        delta1 _ q1 _ = q1

        delta2 [a1,a2] bs _ = a2:one bs -- push dest
        delta2 _ q1 _ = q1

        one = take 1

        delta = delta2 `composeDelta` delta1 `composeDelta` delta0

queue2 = (fromAutomaton Automaton {
    q_0 = (' ',' '),
    delta = delta
    } liftCmd) { gamma = gamma, popBubble = 1 }
    where
        liftCmd Pop = (' ',' ')
        liftCmd (Push a) = ('x',a)
        liftCmd Nop = ('x',' ')

        gamma (' ',' ') = Nothing
        gamma (' ',a) = Just a
        gamma (a,_) = Just a

        delta0 _ (' ',b) (a,_) = (a,b) -- pop dest
        delta0 (' ',_) (_,b) _ = (' ',b) -- pop source
        delta0 _ q1 _ = q1

        delta1 (_,b) (' ',_) (' ',_) = (b,' ') -- push source,dest
        delta1 (_,b) (a,_) _ = (a,b)

        delta = delta1 `composeDelta` delta0
