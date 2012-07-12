{-# LANGUAGE FlexibleInstances #-}

module CA.StackLike.Tape3 where

import CA
import CA.Output
import CA.StackLike
import Data.Maybe

instance MultiShow a => MultiShow [a] where
    multiShow = pad 3 " " . map (pad 1 ' ' . head . multiShow)

stack3 :: (Eq s) => StackLike [s] s
stack3 = (fromAutomaton Automaton {
        q_0 = [],
        delta = delta
    } liftCmd) { gamma = listToMaybe }
    where
        liftCmd (Push a) = [undefined, undefined, a]
        liftCmd Pop = []
        liftCmd Nop = [undefined,undefined]

        delta0 as bs _ = two bs -- push source

        delta1 _ bs (c:_) | length bs <= 1 = bs++[c] -- pop dest
        delta1 _ q1 _ = q1

        delta2 as bs _ | length as <= 1 = drop 1 bs -- pop source
        delta2 _ q1 _ = q1

        delta3 [_,_,a3] bs _ = a3:two bs -- push dest
        delta3 _ q1 _ = q1

        two = take 2

        delta = delta3 `composeDelta` delta2 `composeDelta` delta1 `composeDelta` delta0

instance MultiShow ([Char], Maybe Char) where
    multiShow (cs,d) = map (:[]) $ pad 2 ' ' cs ++ [fromMaybe ' ' d]

queue3 = (fromAutomaton Automaton {
        q_0 = ([],Nothing),
        delta = delta
    } liftCmd) { gamma = listToMaybe . fst }
    where
        liftCmd (Push a) = (['x','x'],Just a)
        liftCmd Pop = ([],Nothing)
        liftCmd Nop = (['x','x'],Nothing)

        delta0 _ (cs,d) (a:_,_) | length cs <= 1 = (cs++[a],d) -- pop dest
        delta0 _ q1 _ = q1

        delta1 (cs1,_) (cs2,d) _ | length cs1 <= 1 = (drop 1 cs2,d)
        delta1 _ q1 _ = q1

        delta2 (_,Just c) (cs,_) _ | length cs <= 1 = (cs++[c],Nothing) -- push source, dest
        delta2 (_,c) (cs,_) _ = (cs,c)

        delta = delta2 `composeDelta` delta1 `composeDelta` delta0
