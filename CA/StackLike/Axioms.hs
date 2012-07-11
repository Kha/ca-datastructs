{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module CA.StackLike.Axioms where

import CA
import CA.StackLike
import CA.StackLike.Tape3
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

prop_push_stack3 = axiom_push stack3 . cmdsToTape stack3
prop_pop_stack3 = axiom_pop stack3 . cmdsToTape stack3
prop_nop_stack3 = axiom_nop stack3 . cmdsToTape stack3

prop_enqueue_queue3 = axiom_enqueue queue3 . cmdsToTape queue3
prop_pop_queue3 = axiom_pop queue3 . cmdsToTape queue3
prop_nop_queue3 = axiom_nop queue3 . cmdsToTape queue3

check = quickCheckWith $ stdArgs { maxSize = 1000 }
