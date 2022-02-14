-- Mitchell Stewart, Matthew Hotchkiss, Alex Vogt
-- CS381 Julianne Coffman
-- 2/12/2022
-- Assignment 5

module StackLang where
import Data.Maybe
--import Data.Either

type Prog = [Cmd]

data Val 
    = I Int 
    | B Bool
    deriving (Eq,Show)

type Stack = [Val]

data Cmd
    = LDI Int
    | LDB Bool
    | LEQ
    | ADD
    | MULT
    | DUP
    | IFELSE Prog Prog
    | DEC
    | SWAP
    | POP Int
    deriving (Eq,Show)

type Rank = Int

type CmdRank = (Int, Int)

-- define testing stack and some tests
stack1::Stack
stack1 = [I 1, I 3, I 5, I 7, I 9]
stack2::Stack
stack2 = [B True, I 3]
test1 = [LDI 3, DUP, ADD, DUP, MULT]
test2 = [LDB True, DUP, IFELSE [LDI 1] [LDI 0]]
test3 = [LEQ]
test4 = [ADD, ADD, MULT, DUP]
test5 = [LEQ, IFELSE [] [], LDI 9]
test6 = [LDI 5, LDI 2, LEQ, IFELSE [LDI 10, DUP] [], ADD]
test7 = [LDI 5, LDI 7, LEQ, IFELSE [LDI 10, DUP] [LDI 20, DUP], ADD]
test8 = [LDI 1, LDI 2, LDI 3, LDI 4, LDI 5, ADD, ADD, ADD, ADD]

intToVal :: [Int] -> Stack
intToVal [] = []
intToVal (x:xs) = ((I x):(intToVal xs))

-- for integers, adds to list
-- for booleans, adds 0 for false and 1 for true
valToInt :: Stack -> [Int]
valToInt [] = []
valToInt ((I x):xs) = (x:(valToInt xs))
valToInt ((B x):xs) = ((fromEnum x):(valToInt xs))

run :: Prog -> [Int] -> Type
run p s = semStatTC p (intToVal s)

--old code below, with the new functions added into semCmd
-------------------------------------------------------------------------------

-- function to run commands
runOld :: Prog -> Stack -> Maybe Stack
-- if there are no commands, then just return the original stack
runOld [] s = Just s
-- if there are commands, run the command on the first command in the array
-- recursively run next command on the updated stack and with the tail of the array as an input
runOld (x:xs) s = (semCmd x s) >>= runOld xs

-- function that executes a command on a stack
semCmd :: Cmd -> Stack -> Maybe Stack
-- loads an integer onto the stack, return the stack with the new element at the front
semCmd (LDI n) s = Just ((I n):s)
-- loads a boolean onto the stack, return the stack with the new element at the front
semCmd (LDB b) s = Just ((B b):s)
-- if the command is add, add the two first elements and put them at the front of the tail
-- if there aren't two elements at the front of the stack, this will not execute
-- only runs on integers, so Right must go in front of inputs
semCmd (ADD) ((I x):(I y):xs) = Just (I (x+y):xs)
-- if the command is multiply, multiply the two first elements and put them at the front of the tail
-- if there aren't two elements at the front of the stack, this will not execute
-- only runs on integers, so Right must go in front of inputs
semCmd (MULT) ((I x):(I y):xs) = Just (I (x*y):xs)
-- if the command is duplicate, put the head of the stack into the stack twice
-- if there is not an element in the stack this will not run
-- runs on integers and booleans
semCmd (DUP) (x:xs) = Just (x:x:xs)
-- if the command is LEQ, check if the first element is less than or equal to the second in the stack
-- if it is, load true into the stack, otherwise load false
-- first two elements must be integers
semCmd (LEQ) ((I x):(I y):xs)
    | x <= y = semCmd (LDB True) xs
    | otherwise = semCmd (LDB False) xs
-- if the command is IFELSE, check truth value of the first element
-- if it's true, run the first passed in Prog, otherwise run the second
-- first element must be a boolean
semCmd (IFELSE p1 p2) ((B x):xs)
    | x==True = runOld p1 xs
    | otherwise = runOld p2 xs

semCmd (DEC) ((I x):xs) = Just ((I (x-1)):xs)

semCmd (SWAP) (x:y:xs) = Just (y:x:xs)

semCmd (POP k) [] = Just []
semCmd (POP 0) s = Just s
semCmd (POP k) (x:xs) = semCmd (POP (k-1)) xs
-- catches any instance where performing a command is invalid and returns nothing
semCmd _ _ = Nothing

-- all completely new stuff
-------------------------------------------------------------------------------------

rankC :: Cmd -> CmdRank
rankC (LDI n) = (0,1)
rankC (LDB b) = (0,1)
rankC (LEQ) = (2,1)
rankC (ADD) = (2,1)
rankC (MULT) = (2,1)
-- no implementation for IFELSE as it is calculated differently (see rank function)
rankC (DUP) = (1,2)
rankC (DEC) = (1,1)
rankC (SWAP) = (2,2)
rankC (POP k) = (k,0)

rankP :: Prog -> Rank -> Maybe Rank
rankP [] r = Just r
rankP (x:xs) r = (rank x r) >>= rankP xs


rank :: Cmd -> Rank -> Maybe Rank
rank (IFELSE p1 p2) r
    | ((rankP p1 (r-1)) == Nothing || (rankP p2 (r-1)) == Nothing) = Nothing
    | fromJust (rankP p1 (r-1)) <= fromJust (rankP p2 (r-1)) = (rankP p1 (r-1))
    | otherwise = (rankP p2 (r-1))
rank c r
    | checkCmdRank (rankC c) r /= -100 = Just (r - checkCmdRank (rankC c) r)
    | otherwise = Nothing

checkCmdRank :: CmdRank -> Rank -> Int
checkCmdRank (x,y) r
    | r >= x = x-y
    | otherwise = -100

data Type
    = A [Int]
    | Int
    | Bool
    | TypeError
    | RankError
    deriving (Eq,Show)

tcVal :: Val -> Type
tcVal (I n) = Int
tcVal (B b) = Bool

tc :: Cmd -> Stack -> Type
tc (LDI n) s = Int
tc (LDB b) s = Bool
tc (LEQ) (x:y:xs)
    | tcVal x == Int && tcVal y == Int = Int
tc (ADD) (x:y:xs)
    | tcVal x == Int && tcVal y == Int = Int
tc (MULT) (x:y:xs)
    | tcVal x == Int && tcVal y == Int = Int
tc (DUP) (x:xs) = tcVal x
tc (IFELSE p1 p2) (x:xs)
    | tcVal x == Bool = Bool
tc (DEC) (x:xs)
    | tcVal x == Int = Int
tc (SWAP) s = Int --doesn't matter the types being swapped
tc (POP k) s = Int --doesn't matter the types being popped
tc _ _ = TypeError

semStatTC :: Prog -> Stack -> Type
semStatTC [] s = A (valToInt s)
semStatTC (x:xs) s
    | rankP (x:xs) (length s) == Nothing = RankError
    | tc x s == TypeError = TypeError
    | otherwise = semStatTC xs (fromJust (semCmd x s))

