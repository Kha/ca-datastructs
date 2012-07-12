{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module CA.StackLike.Axioms where

import CA
import CA.StackLike
import CA.StackLike.Tape3
import CA.StackLike.Tape2
import CA.StackLike.Tape1
import Test.QuickCheck
import Test.QuickCheck.Test

instance Arbitrary (StackCmd Char) where
    arbitrary = frequency [(2,Push `fmap` arbitrary),(1,return Nop),(1,return Pop)]


axiom_push :: (Eq q) => StackLike q Char -> [q] -> Char -> Bool
axiom_push stack tape a = bigGamma stack (deltaPushStar stack a tape) == a : bigGamma stack tape

axiom_enqueue :: (Eq q) => StackLike q Char -> [q] -> Char -> Bool
axiom_enqueue stack tape a = bigGamma stack (deltaPushStar stack a tape) == bigGamma stack tape ++ [a]

axiom_pop :: (Eq q) => StackLike q Char -> [q] -> Bool
axiom_pop stack tape = case deltaPopStar stack tape of
    (Nothing,tape') -> bigGamma stack tape == [] && bigGamma stack tape' == []
    (Just a,tape') -> bigGamma stack tape == a : bigGamma stack tape'

axiom_nop :: (Eq q) => StackLike q Char -> [q] -> Bool
axiom_nop stack tape = bigGamma stack (delta' stack Nop tape) == bigGamma stack tape

cmdsToTape :: (Eq q) => StackLike q s -> [StackCmd s] -> [q]
cmdsToTape stack = foldl (flip $ delta' stack) []

stackProps stack = (f axiom_push,f axiom_pop,f axiom_nop) where
    f axiom = axiom stack . cmdsToTape stack
queueProps stack = (f axiom_enqueue,f axiom_pop,f axiom_nop) where
    f axiom = axiom stack . cmdsToTape stack

check = quickCheckWith $ stdArgs { maxSize = 1000 }

(prop_push_stack3, prop_pop_stack3, prop_nop_stack3) = stackProps stack3
(prop_push_stack2, prop_pop_stack2, prop_nop_stack2) = stackProps stack2
--(rop_push_stack1, rop_pop_stack1, rop_nop_stack1) = stackProps stack1


(prop_enqueue_queue3, prop_pop_queue3, prop_nop_queue3) = queueProps queue3
(prop_enqueue_queue2, prop_pop_queue2, prop_nop_queue2) = queueProps queue2
