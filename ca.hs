import Data.Bits
import Data.Char
import Control.Concurrent
import System.Console.ANSI

data Automaton a = Automaton {
    q_0 :: a,
    delta :: a -> a -> a -> a
}

windowed :: Int -> [a] -> [[a]]
windowed size xs@(x:xs') = case take size xs of
    win | length win == size -> win : windowed size xs'
    _ -> []

diffOut :: String -> String -> IO ()
diffOut x y = loop $ zip (x ++ repeat ' ') y where
    loop [] = putStrLn ""
    loop xs = do
        putStr . map snd $ same
        setSGR [SetColor Foreground Vivid Red]
        putStr . map snd $ unsame
        setSGR [SetColor Foreground Vivid White]
        loop rest'
        where
            (same, rest) = span (uncurry (==)) xs
            (unsame, rest') = span (uncurry (/=)) rest

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

run :: (Eq a, Show a) => Automaton a -> [a] -> Int -> (a -> String) -> IO ()
run a tape padding out = loop a (printTape padding tape) tape padding out where
    loop a lastLine tape padding out = do
        let line = printTape padding tape
        diffOut lastLine line
        threadDelay 300000
        let (tape',padding') = step a tape
        loop a line tape' (padding + padding') out
    printTape padding = take 70 . pad padding . concatMap out
    pad n = drop (-n) . (replicate n ' ' ++)


-- 110


turing = run (rule 110) [1] 60 out where
    out 0 = " "
    out n = show n


-- tape1


data T1Cmd = Nop | Pop | Push Char deriving (Eq)
data Tape1 =
    Cmd T1Cmd
    | Empty
    | Cell (Char, T1Cmd)
    deriving (Eq)

instance Show Tape1 where
    show (Cmd Nop) = " "
    show (Cmd Pop) = "p"
    show (Cmd (Push c)) = [c]
    show Empty = " "
    show (Cell (c, Nop)) = [c]
    show (Cell (c, _)) = [toUpper c]

tape1 cmds = run (Automaton {
    q_0 = Empty,
    delta = delta
    }) (map (Cmd . parse) cmds ++ map (\c -> Cell (c, Nop)) "abcdefghijklmn") 20 show
    where
        delta cmd@(Cmd _) (Cmd _) _ = cmd
        delta Empty (Cmd _) _ = Empty
        delta (Cmd Pop) (Cell _) (Cell (c,_)) = Cell (c, Pop)
        delta (Cell (_, Pop)) (Cell _) (Cell (c,_)) = Cell (c, Pop)
        delta (Cell (_, Pop)) (Cell _) Empty = Empty
        delta (Cmd (Push c')) (Cell (c,_)) _ = Cell (c', Push c)
        delta (Cell (_, Push c')) (Cell (c,_)) _ = Cell (c', Push c)
        delta (Cell (_, Push c')) Empty _ = Cell (c', Nop)
        delta _ (Cell (c,_)) _ = Cell (c, Nop)
        delta _ q1 _ = q1

        parse ' ' = Nop
        parse 'p' = Pop
        parse c   = Push c
