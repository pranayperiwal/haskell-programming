{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use const" #-}

--------------------------------------------
-- Functional Programming (Assignment 3)
--------------------------------------------

-- Name: Pranay Periwal
-- UID : 3035664697

module Assignment3 where

import Parsing -- download Parsing.hs at the Moodle Lecture 7 (no need to submit this file)
import Control.Monad.Trans.State ( get, put, State, evalState )
import Control.Monad ( ap, liftM, liftM2 )
import GHC.Base (liftA)
import Foreign.Marshal.Array (moveArray)
import Data.Bits ( Bits((.|.), (.&.)) )

---------------------------------
-- IO Interaction
---------------------------------

-- Problem 1
type BoardSpace = [(Int, String)]

spaces :: Int -> String
spaces n = replicate n ' '

printGame :: BoardSpace -> IO ()
printGame [] = putStr ""
printGame ((num, str):xs) = do putStrLn (str ++ spaces (6-length str) ++ "| " ++ show num)
                               printGame xs

generateBoard :: Int -> BoardSpace
generateBoard num = [(x, replicate x '*') | x <- [1..num]]

playerMoveRequest :: IO (Int, Int)
playerMoveRequest = do putStr "Enter a row number: "
                       row <- readLn
                       putStr "Stars to remove: "
                       stars <- readLn
                       putStrLn ""
                       return (row, stars)

checkValidMove:: (Int, Int) -> Int -> BoardSpace -> IO ()
checkValidMove (row, stars) player board
                                -- if row is longer than game board
                                | row > length board =
                                    do  putStrLn ""
                                        putStrLn ("Warning: There are only " ++ show (length board) ++ " rows in the game. Try again.")
                                        play player board

                                  -- if stars are more than stas in row
                                | stars > length (snd (board !! (row-1))) =
                                    do  putStrLn ""
                                        putStrLn ("Warning: There are only " ++ show (length (snd (board !! (row-1)))) ++ " stars in the row "++ show (fst (board !! (row-1))) ++ ". Try again.")
                                        play player board

                                | otherwise =
                                              do  printGame $ makeMove (row, stars) board
                                                  if player == 1
                                                    then play 2 (makeMove (row, stars) board)
                                                    else play 1 (makeMove (row, stars) board)



makeMove :: (Int, Int) -> BoardSpace -> BoardSpace
makeMove (row, stars) = map (\(x,y) -> if row == x
                                          then (row, replicate (length y - stars) '*')
                                          else (x,y))

checkWin :: BoardSpace -> Bool
checkWin board = all (==True) [ null stars | (num, stars) <- board]



play :: Int -> BoardSpace -> IO ()
play player board = if player == 1
                      then
                        do
                          putStrLn ""
                          if checkWin board
                            then putStrLn "Player 2 wins!"
                            else
                              do
                                putStrLn "Player 1"
                                move <- playerMoveRequest
                                checkValidMove move player board

                      else
                        do
                          putStrLn ""
                          if checkWin board
                            then putStrLn "Player 1 wins!"
                            else
                              do
                                putStrLn "Player 2"
                                move <- playerMoveRequest
                                checkValidMove move player board

nim :: Int -> IO ()
nim x = do printGame $ generateBoard x
           play 1 (generateBoard x)

-- Problem 2
showMoveAI :: (Int, Int) -> IO ()
showMoveAI (num, stars) = do
                            putStrLn ("Enter a row number: " ++ show num)
                            putStrLn ("Stars to remove: " ++ show stars)

aiMove:: BoardSpace -> (Int, Int) -> (Int,Int)
aiMove [] (curNum, curMax) = (curNum, curMax)
aiMove ((num, stars):xs) (curNum, curMax) = if length stars > curMax
                                                then aiMove xs (num, length stars)
                                                else aiMove xs (curNum, curMax)

checkValidPlayerMove:: (Int, Int) -> BoardSpace -> IO ()
checkValidPlayerMove (row, stars) board
                                -- if row is longer than game board
                                | row > length board =
                                    do  putStrLn ""
                                        putStrLn ("Warning: There are only " ++ show (length board) ++ " rows in the game. Try again.")
                                        playAI 1 board

                                  -- if stars are more than stas in row
                                | stars > length (snd (board !! (row-1))) =
                                    do  putStrLn ""
                                        putStrLn ("Warning: There are only " ++ show (length (snd (board !! (row-1)))) ++ " stars in the row "++ show (fst (board !! (row-1))) ++ ". Try again.")
                                        playAI 1 board

                                | otherwise =
                                              do  printGame $ makeMove (row, stars) board
                                                  playAI 2 (makeMove (row, stars) board)


playAI:: Int -> BoardSpace -> IO ()
playAI player board = if player == 1
                        then
                          do
                          putStrLn ""
                          if checkWin board
                            then putStrLn "AI wins!"
                            else
                              do
                                putStrLn "Player"
                                move <- playerMoveRequest
                                checkValidPlayerMove move board
                        else
                          do
                            putStrLn ""
                            if checkWin board
                              then putStrLn "Player wins!"
                              else
                                do
                                  putStrLn "AI"
                                  let move = aiMove board (0, 0)
                                  showMoveAI move
                                  printGame $ makeMove move board
                                  playAI 1 (makeMove move board)



nimAI :: Int -> IO ()
nimAI x = do
            printGame $ generateBoard x
            playAI 1 (generateBoard x)

---------------------------------
-- Functional Parsing
---------------------------------

data Binop = Add | Sub | Mul | Div | Mod deriving (Eq, Show)

data Expr = Bin Binop Expr Expr
          | Val Int
          | Var String deriving (Eq, Show)

type Env = [(String, Int)]

-- Problem 3

eval :: Env -> Expr -> Maybe Int
eval _ (Val x) = Just x
eval env (Var s) = lookup s env
eval env (Bin Add x y) = (+) <$> eval env x <*> eval env y
eval env (Bin Sub x y) = (-) <$> eval env x <*> eval env y
eval env (Bin Mul x y) = (*) <$> eval env x <*> eval env y
eval env (Bin Div x y) = if eval env y == Just 0 then Nothing else div <$> eval env x <*> eval env y
eval env (Bin Mod x y) = if eval env y == Just 0 then Nothing else mod <$> eval env x <*> eval env y


-- Problem 4

integerVal :: Parser Expr
integerVal  = Val <$> integer

identifierVar :: Parser Expr
identifierVar = Var <$> identifier

expr :: Parser Expr
expr = do t <- token term
          (do opt <- opTerm t
              return opt)
            +++
            return t

term :: Parser Expr
term = do f <- token factor
          (do opf <- opFactor f
              return opf)
            +++
            return f

opTerm :: Expr -> Parser Expr
opTerm e = addParser +++ subParser
            where
            addParser = do token $ char '+'
                           t <- token term
                           (do opt <- opTerm (Bin Add e t)
                               return opt)
                             +++
                             return (Bin Add e t)

            subParser = do token $ char '-'
                           t <- token term
                           (do opTerm (Bin Sub e t))
                             +++
                             return (Bin Sub e t)


opFactor :: Expr -> Parser Expr
opFactor e = mulParser +++ divParser +++ modParser
              where
                mulParser = do token $ char '*'
                               f <- token factor
                               (do opf <- opFactor (Bin Mul e f)
                                   return opf)
                                 +++
                                 return (Bin Mul e f)

                divParser = do   token $ char '/'
                                 f <- factor
                                 (do opf <- opFactor (Bin Div e f)
                                     return opf)
                                   +++
                                   return (Bin Div e f)

                modParser = do   token $ char '%'
                                 f <- factor
                                 (do opf <- opFactor (Bin Mod e f)
                                     return opf)
                                   +++
                                   return (Bin Mod e f)




factor  :: Parser Expr
factor = (do token $ char '('
             e <- expr
             token $ char ')'
             return e)
          +++ integerVal
          +++ identifierVar

pExpr :: Parser Expr
pExpr = do  expr

-- Problem 5

constantMath :: Int -> Int -> Binop -> Int
constantMath x y Add = x + y
constantMath x y Sub = x - y
constantMath x y Mul = x * y
constantMath x y Div = x `div` y
constantMath x y Mod = x `mod` y

optimize :: Expr -> Maybe Expr
optimize (Val x) = Just (Val x)
optimize (Var x) = Just (Var x)
optimize (Bin Mul (Val 0) _) = Just (Val 0)
optimize (Bin Mul _ (Val 0)) = Just (Val 0)
optimize (Bin Div _ (Val 0))  = Nothing
optimize (Bin Mod _ (Val 0))  = Nothing
optimize (Bin Add x (Val 0)) = optimize x
optimize (Bin Add (Val 0) x) = optimize x
optimize (Bin Sub x (Val 0)) = optimize x
optimize (Bin op (Val x) (Val y)) = optimize (Val (constantMath x y op))
optimize (Bin op e1 e2 ) =  if e1 == exp1 && e2 == exp2
                              then Just (Bin op e1 e2)
                              else optimize (Bin op exp1 exp2)
                                where
                                  exp1 = case optimize e1 of
                                    Nothing -> Val 0
                                    Just x -> x
                                  exp2 = case optimize e2 of
                                    Nothing -> Val 0
                                    Just x -> x




---------------------------------
-- Programming with Monads
---------------------------------

-- Problem 6

type EvalState = [Int]
type EvalValue = Int

evalL :: [String] -> State EvalState EvalValue
evalL [] = do xs <- get
              if null xs
                then return 0
                else return (head xs)
evalL (x:xs) = do stack <- get
                  case x of
                    "inc" -> put $ (head stack + 1):tail stack
                    "dec" -> put $ (head stack - 1):tail stack
                    "+" -> put $ (head stack + stack !! 1):drop 2 stack
                    "-" -> put $ (head stack - stack !! 1):drop 2 stack
                    "*" -> put $ (head stack * stack !! 1):drop 2 stack
                    "/" -> put $ (head stack `div` stack !! 1):drop 2 stack
                    "&" -> put $ (head stack .&. stack !! 1):drop 2 stack
                    "|" -> put $ (head stack .|. stack !! 1):drop 2 stack
                    "clear" -> put []
                    "dup" -> put $ head stack:head stack:tail stack
                    num -> put $ (read num :: Int):stack
                  evalL xs

solveRPN :: String -> Int
solveRPN xs = evalState (evalL . words $ xs) []

-- Problem 7


newtype Stack a = Stack {runStack :: [Int] -> ([Int], a)}

instance Functor Stack where
    fmap = liftM

instance Applicative Stack where
    pure x = Stack $ \s -> (s, x)
    (<*>) = ap

instance Monad Stack where
    return = pure
    m >>= k = Stack $ \s -> case runStack m s of
        (s', x) -> runStack (k x) s'


pop :: Stack Int
pop = Stack (\xs -> if null xs then ([], 0) else (tail xs, head xs))

push :: Int -> Stack Int
push x = Stack (\xs -> (x:xs, x))

evalStack :: Stack Int -> [Int] -> Int
evalStack m s = snd (runStack m s)

evalL' :: [String] -> Stack Int
evalL' [] = pop
evalL' (x:xs) = do
                  case x of
                    "inc" -> do x <- pop
                                push (x + 1)
                    "dec" -> do x <- pop
                                push (x - 1)
                    "+" -> do x <- pop
                              y <- pop
                              push (x + y)
                    "-" -> do x <- pop
                              y <- pop
                              push (x - y)
                    "*" -> do x <- pop
                              y <- pop
                              push (x * y)
                    "/" -> do x <- pop
                              y <- pop
                              push (x `div` y)
                    "&" -> do x <- pop
                              y <- pop
                              push (x .&. y)
                    "|" -> do x <- pop
                              y <- pop
                              push (x .|. y)
                    "dup" -> do x <- pop
                                push x
                                push x
                    "clear" -> Stack (\_ -> ([], 0))
                    num -> push (read num :: Int)
                  evalL' xs

solveRPN' :: String -> Int
solveRPN' xs = evalStack (evalL' . words $ xs) []



-- Problem 8

safeLog :: (Ord a, Floating a) => a -> Maybe a
safeLog x
      | x > 0      = Just (log x)
      | otherwise  = Nothing

safeSqrt :: (Ord a, Floating a) => a -> Maybe a
safeSqrt x
        | x >= 0    = Just (sqrt x)
        | otherwise = Nothing


safeLogSqrt :: (Ord a, Floating a) => a -> Maybe a
safeLogSqrt x =  safeSqrt x >>= safeLog

-- Problem 9

zipL :: [a] -> [b] -> [(a, b)]
zipL [] _ = []
zipL _ [] = []
zipL (x:xs) (y:ys) = return (x, y) ++ zipL xs ys


-- Problem 10 (Advanced)

newtype LM a = LM { getLM :: [Maybe a] }
  deriving Functor


instance Applicative LM where
  pure :: a -> LM a
  pure = return
  (<*>) = liftM2 ($)

instance Monad LM where
  return :: a -> LM a
  return x = LM [Just x]

  (>>=) :: LM a -> (a -> LM b) -> LM b
  ms >>= f =  LM $ concatMap (\m -> case m of
                                     Nothing -> [Nothing]
                                     Just x -> getLM (f x)
                                  ) (getLM ms)


{-
---------- Monad Laws Reasoning ----------------
Law 1: (return x) >>= f   ===   f x
LHS: 
= {by defition of return}
  LM [Just a] >>= f
= {by definition of (>>=)}
    LM $ concatMap (\m -> case m of
                                     Nothing -> [Nothing]
                                     Just x -> getLM (f x)
                                  ) (getLM LM [Just a])
= {by definition of getLM}
<=> LM $ concatMap (\m -> case m of
                                     Nothing -> [Nothing]
                                     Just x -> getLM (f x)
                                  ) [Just a]
<=> LM $ (getLM (f a))
= {by definition of f}
<=> LM $ (getLM (LM y))
= {by definition of getLM}
<=> LM $ [Just y]
= {by definition of LM}
<=> LM y

RHS:
= {by definition of f}
    f a
<=> LM y
∴ LHS = RHS (Proved)

----------------------------------------------------------------
Law 2: ms >>= return   ===   ms

LHS:

= {by definition of (>>=)}
<=> LM $ concatMap (\m -> case m of
                            Nothing -> [Nothing]
                            Just x -> getLM (return x)
                         ) (getLM ms)

= {Assuming ms = LM x, and by definition of getLM}
<=> LM $ concatMap (\m -> case m of
                            Nothing -> [Nothing]
                            Just x -> getLM (return x)
                          ) ([Just x or Nothing])

= {By case analysis on m}
  1. m = [Just x]
<=> LM $ concatMap (\m -> case m of
                            Nothing -> [Nothing]
                            Just x -> getLM (return x)
                          ) ([Just x])
<=> LM $ (getLM (return x))

= {by definition of return}
<=> LM $ ([Just x])

= {by definition of LM}
<=> LM x

  2. m = [Nothing]

<=> LM $ concatMap (\m -> case m of
                            Nothing -> [Nothing]
                            Just x -> getLM (f x)
                          ) ([Just x])
<=> LM $ Nothing
<=> LM x

RHS: ms
= {By defintion of ms = LM x}
<=> LM x
∴ LHS = RHS (Proved)

----------------------------------------------------------------
Law 3: ms >>= (\x -> f x >>= g)       ===     (ms >>= f) >>= g

LHS:
    ms >>= (\x -> f x >>= g)

= {By defintion of (>>=)}
<=> LM $ concatMap (\m -> case m of
                            Nothing -> [Nothing]
                            Just x -> getLM ((\x -> f x >>= g) x)
                         ) (getLM ms)

= {Assuming ms = LM x, and by definition of getLM}
<=> LM $ concatMap (\m -> case m of
                            Nothing -> [Nothing]
                            Just x -> getLM ((\x -> f x >>= g) x)
                          ) ([Just x or Nothing])

= {By case analysis on m}
  1. m = [Just x]
<=> LM $ concatMap (\m -> case m of
                            Nothing -> [Nothing]
                            Just x -> getLM ((\x -> f x >>= g) x)
                          ) ([Just x])
<=> LM $ (getLM ((\x -> f x >>= g) x))

= {By defintion of (>>=)}
<=> LM $ (getLM (LM v))

= {by definition of getLM}
<=> LM [Just v]

= {by definition of LM}
<=> LM v

  2. m = [Nothing]
<=> LM $ concatMap (\m -> case m of
                            Nothing -> [Nothing]
                            Just x -> getLM ((\x -> f x >>= g) x)
                          ) ([Just x])
<=> LM $ [Nothing]

= {by definition of LM}
<=> LM v

RHS: (ms >>= f) >>= g

= {By definition of (>>=)}
<=> LM y >>= g

= {By definition of (>>=)}
<=> LM v

∴ LHS = RHS (Proved)

-------------------------------------------------
-}

newtype ML a = ML { getML :: Maybe [a] }
  deriving (Functor, Show)

instance Applicative ML where
  pure = return
  (<*>) = liftM2 ($)

instance Monad ML where
  return :: a -> ML a
  return x = ML $ Just [x]

  (>>=) :: ML a -> (a -> ML b) -> ML b
  ML Nothing >>= _   = ML Nothing
  ML (Just xs) >>= f = ML $ Just $ concatMap
                                  (\x -> case f x of
                                      ML Nothing -> []
                                      ML (Just ys) -> ys) xs


{-
---------- Monad Laws Reasoning ----------------
Law 1: (return x) >>= f   ===   f x

      LHS: 
      <=> (return x) >>= f

      = {By defintion of return }
      <=>  ML $ Just xs  >>= f

      = {By definition of (>>=)}
      <=> ML $ Just $ concatMap 
                        (\x -> case f x of
                            ML Nothing -> []
                            ML (Just ys) -> ys) xs

      = {By case analysis on f x}
      1. f x = ML Nothing
      <=> ML $ Just $ []

      = {By definition of ML}
      <=> ML a

      2. f x = ML (Just ys)
      <=> ML $ Just ys

      = {By definition of ML}
      <=> ML a

      RHS:
      <=> f x
      = {By case analysis on f x}

      1. f x = ML Nothing
      <=> ML $ Just $ []

      = {By definition of ML}
      <=> ML a

      2. f x = ML (Just ys)
      <=> ML $ Just ys

      = {By definition of ML}
      <=> ML a

      ∴ LHS = RHS (Proved)

----------------------------------------------------------------

Law 2: m >>= return      ===      m
      
      LHS:
      <=> m >>= return
      
      = {By case analysis of (>>=)}
      1. m = ML Nothing 

      = {By definition of (>>=)}
      <=> ML Nothing

      2. m = ML (Just xs)

      = {By definition of (>>=)}
      <=> ML (Just xs) >>= f 
      <=> ML $ Just $ concatMap 
                        (\x -> case f x of
                            ML Nothing -> []
                            ML (Just ys) -> ys) xs
      
      = {By case analysis on f x}
      1. f x = ML Nothing
      <=> ML $ Just $ []

      = {By definition of ML}
      <=> ML a

      2. f x = ML (Just ys)
      <=> ML $ Just ys

      = {By definition of ML}
      <=> ML a

    RHS: m
    = {By possibility of m = ML Nothing or m = ML (Just ys)}
    1. m =  ML Nothing
    2. m =  ML (Just ys)
        <=> ML a

    ∴ LHS = RHS (Proved)
  
----------------------------------------------------------------

Law 3: ms >>= (\x -> f x >>= g)       ===     (ms >>= f) >>= g

    LHS: 
        ms >>= (\x -> f x >>= g)

    = {By case analysis on (>>=)}
        1. ms = ML Nothing
    <=> ML Nothing >>= (\x -> f x >>= g)  
    
    = {By definition of (>>=)}
    <=> ML Nothing 

        2. ms = ML (Just xs)
    <=> ML (Just xs) >>= (\x -> f x >>= g) 
    
    = {By definition of (>>=)} 
    <=> ML $ Just $ concatMap 
                      (\x -> case (\x -> f x >>= g) x of
                          ML Nothing -> []
                          ML (Just ys) -> ys) xs

    = {By case analysis on (\x -> f x >>= g)}
      {f x returns ML p, (ML p) >>= g returns (ML q) }
      
      1. (\x -> f x >>= g) = ML Nothing   
      <=> ML $ Just $ []

      = {By definition of ML}
      <=> ML a

      2. (\x -> f x >>= g) = ML (Just ys)
      <=> ML $ Just ys

      = {By definition of ML}
      <=> ML a


    RHS: 
        (ms >>= f) >>= g
      = {By case analysis on (>>=)}

      1.  ms = ML Nothing
      <=> (ML Nothing >>= f) >>= g

      = {By definition of ML Nothing >>= _ }
      <=> ML Nothing >>= g

      = {By definition of ML Nothing >>= _ }
      <=> ML Nothing 

      2.  ms = ML (Just xs)
      <=> (ML (Just xs) >>= f) >>= g

      = {By definition of ML (Just xs) >>= f}
      <=> (ML $ Just $ concatMap 
                          (\x -> case f x of
                              ML Nothing -> []
                              ML (Just ys) -> ys) xs) >>= g
      
      = {By definition of ML}
      <=> (ML b) >>= g

       = {By definition of ML b >>= g}
      <=> (ML $ Just $ concatMap 
                          (\x -> case g x of
                              ML Nothing -> []
                              ML (Just ys) -> ys) xs) 
      
      = {By definition of ML}
      <=> ML a

      ∴ LHS = RHS (Proved)

-------------------------------------------------
-}