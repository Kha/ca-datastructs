{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}


module CA.Samples where

import CA
import CA.Output
import Data.Bits

-- 110

instance MultiShow Int where
    multiShow 0 = [" "]
    multiShow x = [show x]

rule :: Int -> Automaton Int
rule n = Automaton {
        q_0 = 0,
        delta = delta
    } where
        delta q0 q1 q2 = if testBit n (q0*4+q1*2+q2) then 1 else 0

turing = run (rule 110,[1]) 60

-- reverse

instance MultiShow (Maybe Char) where
    multiShow (Just c) = multiShow c
    multiShow Nothing  = [" "]

instance (MultiShow a, MultiShow b) => MultiShow (a,b) where
    multiShow (x,y) = multiShow x ++ multiShow y

reverseString s = run (Automaton {
    q_0 = (Nothing,Nothing),
    delta = delta
    },map ((, Just ' ') . Just) s) 20
    where
        deltaTop :: Delta (Maybe Char)
        deltaTop _ (Just ' ') (Just c) = Just c
        deltaTop (Just ' ') (Just _) _ = Just ' '
        deltaTop _ q0 _ = q0

        deltaCrossover :: Delta (Maybe Char, Maybe Char)
        deltaCrossover (Nothing,Nothing) (Just c,Just ' ') _ = (Just ' ',Just c)
        deltaCrossover _ q0 _ = q0

        delta = deltaCrossover `eitherDelta` (deltaTop `cartesianDelta` reverseDelta deltaTop)
