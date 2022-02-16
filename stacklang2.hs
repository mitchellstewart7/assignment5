-- Mitchell Stewart, Matthew Hotchkiss, Alex Vogt
-- CS381 Julianne Coffman
-- 2/12/2022
-- Assignment 5

module StackLang where
import Data.Maybe

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

-- converts a list of integers into a list of vals (which is a stack)
intToVal :: [Int] -> Stack
intToVal [] = []
intToVal (x:xs) = ((I x):(intToVal xs))

-- for integers, adds to list
-- for booleans, adds 0 for false and 1 for true
valToInt :: Stack -> [Int]
valToInt [] = []
valToInt ((I x):xs) = (x:(valToInt xs))
valToInt ((B x):xs) = ((fromEnum x):(valToInt xs))

-- function to run a program on a stack
-- converts the list of integers that are inputted into vals and then runs semStatTC
run :: Prog -> [Int] -> Type
run p s = semStatTC p (intToVal s)

-- OLD FUNCTION, ONLY USED FOR IFELSE IN SEMCMD
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
-- only runs on integers, so I must go in front of inputs
semCmd (ADD) ((I x):(I y):xs) = Just (I (x+y):xs)
-- if the command is multiply, multiply the two first elements and put them at the front of the tail
-- if there aren't two elements at the front of the stack, this will not execute
-- only runs on integers, so I must go in front of inputs
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
-- function to decrement the leading val on the stack
-- only decrements if the leading val is an integer
semCmd (DEC) ((I x):xs) = Just ((I (x-1)):xs)
-- function that swaps the leading two vals on the stack
-- does not matter if vals are integers or booleans
semCmd (SWAP) (x:y:xs) = Just (y:x:xs)
-- function that pops k vals off the stack
-- remove elements off the stack until k is 0
semCmd (POP 0) s = Just s
semCmd (POP k) (x:xs) = semCmd (POP (k-1)) xs
-- catches any instance where performing a command is invalid and returns nothing
-- this is how we identify type errors
semCmd _ _ = Nothing

-- function that determines how many elements function takes off the stack and how many it puts on
-- first int in pair is vals taken off the stack, second int is vals put on the stack
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

-- function to determine rank of input stack after a prog is run on it
rankP :: Prog -> Rank -> Maybe Rank
-- no commands mean rank does not change
rankP [] r = Just r
-- run rank on each command in prog
rankP (x:xs) r = (rank x r) >>= rankP xs

-- function to determine the effect on stack rank of one command
rank :: Cmd -> Rank -> Maybe Rank
-- separate instance for IFELSE
-- checks if either of the progs contains a rank error first
-- if neither do, find the minimum rank of the two progs and return it
rank (IFELSE p1 p2) r
    | ((rankP p1 (r-1)) == Nothing || (rankP p2 (r-1)) == Nothing) = Nothing
    | fromJust (rankP p1 (r-1)) <= fromJust (rankP p2 (r-1)) = (rankP p1 (r-1))
    | otherwise = (rankP p2 (r-1))
-- for commands besides IFELSE
-- run checkCmdRank for the command
-- if checkCmdRank returns -100. we know there is a rank error (return Nothing)
-- if not, subtract the value returned by checkCmdRank from the current stack rank and return the new rank
rank c r
    | checkCmdRank (rankC c) r /= -100 = Just (r - checkCmdRank (rankC c) r)
    | otherwise = Nothing
-- function that takes a CmdRank and a current rank and returns the new rank of a stack with rank r
-- if r is not >= x (the number of vals the command takes off the stack) then return -100 (Nothing)
-- if r is >= x, then return x-y (the net number of elements the rank decreases by) 
checkCmdRank :: CmdRank -> Rank -> Int
checkCmdRank (x,y) r
    | r >= x = x-y
    | otherwise = -100

-- declaration of a type that we will use for returns of the program as a whole
data Type
    = A [Int]
    | TypeError
    | RankError
    deriving (Eq,Show)

-- function that performs rank checking and type checking and returns a type
semStatTC :: Prog -> Stack -> Type
-- if run on an empty prog, convert the stack to a list of integers and return with type A [Int]
semStatTC [] s = A (valToInt s)
-- if there is a list of progs
-- perform rank checking statically on the current stack, if there is a rank error return this
-- perform type checking dynamically by running semCmd (which we know cannot have a rank error as we already checked this)
-- if there is not a rank error or type error, run semcmd on with the current command and stack
-- insert the updated prog and stack into the function again and run recursively
semStatTC (x:xs) s
    | rankP (x:xs) (length s) == Nothing = RankError
    | semCmd x s == Nothing = TypeError
    | otherwise = semStatTC xs (fromJust (semCmd x s))

