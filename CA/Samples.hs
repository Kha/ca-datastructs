{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module CA.Samples where

import CA
import CA.Output
import CA.StackLike
import CA.StackLike.Tape3
import CA.StackLike.Tape2
import CA.StackLike.Tape1
import Data.Bits
import Data.Maybe
import Data.Either

-- 110

--instance MultiShow Int where
--    multiShow 0 = [" "]
--    multiShow x = [show x]

rule :: Int -> Automaton Int
rule n = Automaton {
        q_0 = 0,
        delta = delta
    } where
        delta q0 q1 q2 = if testBit n (q0*4+q1*2+q2) then 1 else 0

rule110 = run (rule 110) [1] 60

-- reverse

instance MultiShow (Maybe Char) where
    multiShow (Just c) = multiShow c
    multiShow Nothing  = [" "]

instance (MultiShow a, MultiShow b) => MultiShow (a,b) where
    multiShow (x,y) = multiShow x ++ multiShow y

reverseString s = run Automaton {
    q_0 = (Nothing,Nothing),
    delta = delta
    } (map ((, Just ' ') . Just) s) 20
    where
        deltaTop :: Delta (Maybe Char)
        deltaTop _ (Just ' ') (Just c) = Just c
        deltaTop (Just ' ') (Just _) _ = Just ' '
        deltaTop _ q0 _ = q0

        deltaCrossover :: Delta (Maybe Char, Maybe Char)
        deltaCrossover (Nothing,Nothing) (Just c,Just ' ') _ = (Just ' ',Just c)
        deltaCrossover _ q0 _ = q0

        delta = deltaCrossover `eitherDelta` (deltaTop `cartesianDelta` reverseDelta deltaTop)

-- Turing machine

data Dir = L | N | R
data DTM q s = DTM {
    qs_T :: [q],
    sigma_T :: [s],
    blank_T :: s,
    delta_T :: q -> s -> (q,s,Dir),
    q_0_T :: q,
    q_f_T :: q
}

instance (MultiShow q', Show q, Show s) => Tape (Either q' (Either (q,s) q')) where
    tapeShow = padTranspose . map mmultiShow where
        mmultiShow (Left q') = multiShow q'
        mmultiShow (Right (Left (q,a))) = [show a, show q]
        mmultiShow (Right (Right q')) = multiShow q'

simulateDTM :: (Eq q, Eq s, Eq q') => DTM q s -> StackLike q' s -> [s] -> AutWithTape (Either q' (Either (q,s) q'))
simulateDTM tm stack tape = (a,tape') where
    a = concatAutomata revStack (concatAutomata head (aut stack) undefined delta1) deltaM1 delta0
    tape' = [Left (q_0 $ aut stack), Right (Left (q_0_T tm,blank_T tm)), Right (Right (q_0 $ aut stack))] --Right (Left (q_0_T tm,fromMaybe (blank_T tm) $ listToMaybe tape)) : map (Right . Right)
    revStack = Automaton { q_0 = (q_0 $ aut stack), delta = \q0 q1 q2 -> (delta $ aut stack) q2 q1 q0 }

    head = Automaton { q_0 = (q_0_T tm,blank_T tm), delta = \q0 q1 q2 -> q1 }

    delta0 qM1 (Left (q,a)) (Right q1) =
        Left $ case (delta_T tm) q a of
            (q',a',N) -> (q',a')
            (q',a',L) -> (q',gamma' qM1)
            (q',a',R) -> (q',gamma' q1)
    delta0 _ _ _ = error ""

    delta1 (q,a) q1 q2 =
        let cmd = case (delta_T tm) q a of
                  (q',a',N) -> Nop
                  (q',a',L) -> Push a'
                  (q',a',R) -> Pop in
        (cell1 stack) cmd q1 q2

    deltaM1 qM2 qM1 (Left (q,a)) =
        let cmd = case (delta_T tm) q a of
                  (q',a',N) -> Nop
                  (q',a',L) -> Pop
                  (q',a',R) -> Push a' in
        (cell1 stack) cmd qM1 qM2

    gamma' = fromMaybe (blank_T tm) . gamma stack

data BusyState = A | B | C | H deriving (Show, Eq)

busy3 :: DTM BusyState Int
busy3 = DTM { qs_T = [A,B,C,H], sigma_T = [0,1], blank_T = 0, delta_T = delta, q_0_T = A, q_f_T = H } where
    delta A 0 = (B,1,R)
    delta A 1 = (H,1,R)
    delta B 0 = (C,0,R)
    delta B 1 = (B,1,R)
    delta C 0 = (C,1,L)
    delta C 1 = (A,1,L)
    delta H a = (H,a,N)

runBusy3 = runWithOptions a True printTape tape 0 where
    (a,tape) = simulateDTM busy3 stack3 []
    printTape _ tape = map (replicate (10 - length left) ' ' ++) . bracketizeLines $ [left ++ show a ++ right, replicate (length left) ' ' ++ show q ++ replicate (length right) ' '] where
        left = reverse . concat . map (concat . map show) . reverse . lefts $ tape
        right = concat . map (concat . map show) . rights . rights $ tape
        (q,a) = head . lefts . rights $ tape

runBusy3' = run a tape 5 where
    (a,tape) = simulateDTM busy3 stack3 []

fixThis :: (Eq a) => (a -> a) -> a -> a
fixThis f x | x == f x  = x
            | otherwise = fixThis f (f x)

runInteractive stack = interactiveStackLike stack . fixThis (delta' stack Nop) . pushMany stack "proseminar" $ []
