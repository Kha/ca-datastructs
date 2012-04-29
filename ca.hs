import Data.Bits
import Data.Char
import Control.Concurrent

data Automaton a = Automaton {
	q_0 :: a,
	delta :: a -> a -> a -> a
}

windowed :: Int -> [a] -> [[a]]
windowed size xs@(x:xs') = case take size xs of
	win | length win == size -> win : windowed size xs'
	_ -> []

step :: Eq a => Automaton a -> [a] -> ([a], Int)
step Automaton { q_0 = q_0, delta = delta } qs =
	let qsPadded = [q_0,q_0] ++ qs ++ [q_0,q_0] in
	let qs' = map (\[q0,q1,q2] -> delta q0 q1 q2) . windowed 3 $ qsPadded in
	let pad = (length . takeWhile (== q_0) $ qs') - 1 in
	(reverse . dropWhile (== q_0) . reverse . dropWhile (== q_0) $ qs', pad)

rule :: Int -> Automaton Int
rule n = Automaton {
		q_0 = 0,
		delta = delta
	} where
		delta q0 q1 q2 = if testBit n (q0*4+q1*2+q2) then 1 else 0

run a tape padding out =
	do
		putStrLn . take 70 . pad padding . concatMap out $ tape
		threadDelay 300000
		let (tape',padding') = step a tape
		run a tape' (padding + padding') out
	where
		pad n = drop (-n) . (replicate n ' ' ++)
 
turing = run (rule 110) [1] 60 out where
	out 0 = " "
	out n = show n

tape1 cmds = run (Automaton {
	q_0 = '!',
	delta = delta
	}) (cmds ++ "|abcdefghijklmn") 20 out
	where
		delta q0 ' ' _ = q0
		delta q0 'p' _ = q0
		delta 'p' '|' q2 = '$'
		delta _ '$' _ = '|'
		delta '$' q1 q2 = toUpper q2
		delta q0 q1 q2 | isUpper q0 = toUpper q2
		delta _ q1 _ | isUpper q1 = toLower q1
		delta _ q1 _ = q1

		out c = [c]
