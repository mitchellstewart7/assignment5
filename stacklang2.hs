-- Mitchell Stewart
-- CS381 Julianne Coffman
-- 2/5/2022
-- Assignment 4

module StackLang where
import Data.Maybe
import Data.Either

type Prog = [Cmd]
type Stack = [Either Bool Int]

data Cmd
    = LDI Int
    | LDB Bool
    | LEQ
    | ADD
    | MULT
    | DUP
    | IFELSE Prog Prog
    deriving Show

-- define testing stack and some tests
stack1::Stack
stack1 = [Right 1, Right 3, Right 5, Right 7, Right 9]
stack2::Stack
stack2 = [Left True, Right 3]
test1 = [LDI 3, DUP, ADD, DUP, MULT]
test2 = [LDB True, DUP, IFELSE [LDI 1] [LDI 0]]
test3 = [LEQ]
test4 = [ADD, ADD, MULT, DUP]
test5 = [LEQ, IFELSE [] [], LDI 9]
test6 = [LDI 5, LDI 2, LEQ, IFELSE [LDI 10, DUP] [], ADD]
test7 = [LDI 5, LDI 7, LEQ, IFELSE [LDI 10, DUP] [LDI 20, DUP], ADD]
test8 = [LDI 1, LDI 2, LDI 3, LDI 4, LDI 5, ADD, ADD, ADD, ADD]

-- function to run commands
run :: Prog -> Stack -> Maybe Stack
-- if there are no commands, then just return the original stack
run [] s = Just s
-- if there are commands, run the command on the first command in the array
-- recursively run next command on the updated stack and with the tail of the array as an input
run (x:xs) s = (semCmd x s) >>= run xs

-- function that executes a command on a stack
semCmd :: Cmd -> Stack -> Maybe Stack
-- loads an integer onto the stack, return the stack with the new element at the front
semCmd (LDI n) s = Just ((Right n):s)
-- loads a boolean onto the stack, return the stack with the new element at the front
semCmd (LDB b) s = Just ((Left b):s)
-- if the command is add, add the two first elements and put them at the front of the tail
-- if there aren't two elements at the front of the stack, this will not execute
-- only runs on integers, so Right must go in front of inputs
semCmd (ADD) ((Right x):(Right y):xs) = Just (Right (x+y):xs)
-- if the command is multiply, multiply the two first elements and put them at the front of the tail
-- if there aren't two elements at the front of the stack, this will not execute
-- only runs on integers, so Right must go in front of inputs
semCmd (MULT) ((Right x):(Right y):xs) = Just (Right (x*y):xs)
-- if the command is duplicate, put the head of the stack into the stack twice
-- if there is not an element in the stack this will not run
-- runs on integers and booleans
semCmd (DUP) (x:xs) = Just (x:x:xs)
-- if the command is LEQ, check if the first element is less than or equal to the second in the stack
-- if it is, load true into the stack, otherwise load false
-- first two elements must be integers
semCmd (LEQ) ((Right x):(Right y):xs)
    | x <= y = semCmd (LDB True) xs
    | otherwise = semCmd (LDB False) xs
-- if the command is IFELSE, check truth value of the first element
-- if it's true, run the first passed in Prog, otherwise run the second
-- first element must be a boolean
semCmd (IFELSE p1 p2) ((Left x):xs)
    | x==True = run p1 xs
    | otherwise = run p2 xs
-- catches any instance where performing a command is invalid and returns nothing
semCmd _ _ = Nothing
