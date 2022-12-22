module Assignment1 where
import Data.Bits ( Bits((.|.), (.&.)) )

import Prelude hiding (and, concat, replicate,  elem, unzip)


--Problem 1
pentanacci :: Int -> Int
pentanacci 1 = 0
pentanacci 2 = 0
pentanacci 3 = 0
pentanacci 4 = 0
pentanacci 5 = 1
pentanacci n = pentanacci (n-1) + pentanacci (n-2) + pentanacci (n-3) + pentanacci (n-4) + pentanacci (n-5)

--Problem 2
solve :: Int -> Int -> [Int]
solve a 1 = [a]
solve a b = (a `quot` b) : solve (a - (a `quot` b)) (b-1)


--Problem 3
compareString :: String -> String -> Bool
compareString str1 ""   = True
compareString "" str2   = False
compareString str1 str2 = (take 1 str1 == take 1 str2) && compareString (tail str1) (tail str2)

removeHyphen :: String -> String
removeHyphen = map (\c -> if c=='-' then ' '; else c)

removeHyphenList :: [String] -> [String]
removeHyphenList = map (map (\c -> if c=='-' then ' '; else c))

lookupName :: [String] -> String -> [String]
lookupName xs ""      = removeHyphenList xs
lookupName [] str     = []
lookupName (x:xs) prefix = if compareString (removeHyphen x) prefix
                             then removeHyphen x : lookupName xs prefix
                             else lookupName xs prefix

--Problem 4
g :: Bool -> Bool -> Bool
g True True = True
g True False = True
g False True = False
g False False = False

sortp :: [a] -> (a -> a -> Bool) -> [a]
sortp [] p = []
sortp [x] p = [x]
sortp (x:xs) p = sortp smaller p ++ [x] ++ sortp larger p
                where
                    smaller = [a | a <- xs, p a x]
                    larger  = [b | b <- xs, not (p b x)]

--Problem 5
foldlp :: (b -> Bool) -> (b -> a -> b) -> b -> [a] -> b
foldlp p f ini []     = ini
foldlp p f ini (x:xs) = if p (f ini x)
                            then foldlp p f (f ini x) xs
                            else ini

--Problem 6
listOrdered :: (a -> a -> Bool) -> [a] -> Bool
listOrdered f [] = True
listOrdered f [x] = True
listOrdered f (x:y:xs) = f x y && listOrdered f xs


isTwinPaired :: [Int] -> Bool
isTwinPaired [] = True
isTwinPaired [x] = True
isTwinPaired xs = listOrdered (<=) even && listOrdered (>=) odd
                where
                    even = [a | a <- xs, a `rem` 2 == 0]
                    odd  = [b | b <- xs, b `rem` 2 == 1]

--Problem 7 
inc :: Int -> Int
inc = (+1)

dec :: Int -> Int
dec x = x - 1

solveRPN :: String -> Int
solveRPN "" = 0
solveRPN str = head (foldl operations [] (words str))
                where   operations (x:y:ys) "+"    = (y + x):ys
                        operations (x:y:ys) "-"    = (y - x):ys
                        operations (x:y:ys) "*"    = (y * x):ys
                        operations (x:y:ys) "/"    = (y `div` x):ys
                        operations (x:y:ys) "&"    = (.&.) y x:ys
                        operations (x:y:ys) "|"    = (.|.) y x:ys
                        operations (x:xs) "inc"    = inc x:xs
                        operations (x:xs) "dec"    = dec x:xs
                        operations (x:xs) "dup"    = x:x:xs
                        operations (x:xs) "clear"  = xs
                        operations xs num          = read num:xs

-- add' :: Int -> Int -> Int
-- add' x y = x+y

-- signum1 :: Int -> Int
-- signum1 n | n>0      = 1
--           | n==0     = 0
--           | otherwise = -1

-- xorr :: Bool -> Bool -> Bool
-- xorr True a = a
-- xorr False a = not a

-- safetail :: [a] -> [a]
-- safetail xs = if null xs then [] else tail xs
-- safetail xs | null xs     = []
--             | otherwise   = tail xs

-- safetail [] = []
-- safetail (x:xs) = xs

-- orr :: Bool -> Bool -> Bool
-- orr True _ = True
-- orr _ _    = False

-- andd :: Bool -> Bool -> Bool
-- andd a b = if a /= b 
--             then False 
--             else a

-- (+++) :: [a] -> [a] -> [a]
-- (+++) xs [] = xs
-- (+++) [] xs = xs
-- (+++) (x:xs) ys = x: (xs +++ ys)

-- and :: [Bool] -> Bool
-- and [] = True
-- and [x] = x
-- and (x:xs) = x && and xs

-- concat :: [[a]] -> [a] 
-- concat [] = []
-- concat [x] = x
-- concat (x:xs) = x ++ concat xs

-- replicate :: Int -> a -> [a]
-- replicate 0 x = []
-- replicate i x = x : replicate (i-1) x

-- (!!) :: [a] -> Int -> a 
-- (!!) [] i     = error "not implemented"
-- (!!) (x:xs) 1 = x
-- (!!) (x:xs) i = (!!) xs (i-1)

-- elem :: Eq a => a -> [a] -> Bool
-- elem i [] = False
-- elem i (x:xs) = x == i || elem i xs

-- merge :: Ord a => [a] -> [a] -> [a]
-- merge [] [] = []
-- merge [] xs = xs
-- merge xs [] = xs
-- merge (x:xs) (y:ys) = if x<=y
--                         then x : merge xs (y:ys)
--                         else y : merge (x:xs) ys

-- msort :: Ord a => [a] -> [a]
-- msort [] = []
-- msort [x] = [x]
-- msort xs = merge a b
--             where
--                 a = msort (take (length xs `div` 2) xs)
--                 b = msort (drop (length xs `div` 2) xs)

-- concat :: [[a]] ->  [a]
-- concat [] = []
-- concat xss = [y | x <- xss, y<-x]

-- prime:: Int -> Bool
-- prime n = length a == 2
--             where
--                 a = [x | x <- [1..n], n `mod` x == 0]

-- positions :: Eq a => a -> [a] -> [Int]
-- positions x [] = []
-- -- positions x xs = [i | i<-[0..(length xs -1)], (!!) xs i == x ]
-- positions x xs = [i | (y,i) <- a, y == x ]
--                 where
--                     a = zip xs [0..(length xs -1)]


-- pyths :: Int -> [(Int,Int,Int)]
-- pyths 0 = []
-- pyths n = [(x, y, z)| z<-[1..n], x<-[1..z], y<-[1..z], z^2 == (x^2 + y^2) ]

-- factors :: Int -> [Int] 
-- factors 0 = []
-- factors n = [ x | x<-[1..(n-1)], n `mod` x == 0 ]

-- perfects:: Int -> [Int]
-- perfects 0 = []
-- perfects n = [ x | x<- [1..n], x == sum (factors x)]

-- scalorProd :: [Int] -> [Int] -> Int
-- scalorProd [] [] = 0
-- scalorProd xs ys = sum [x*y | (x,y)<- zip xs ys]


-- filter :: (a -> Bool) -> [a] -> [a]
-- filter p [] = []
-- filter p xs = [x | x<-xs, p x]

-- len :: [a] -> Int
-- len = foldr (\_ n -> n +1) 0 

-- reverse :: [a] -> [a]
-- reverse [] = []
-- reverse xs = foldr (\x xs -> xs ++ [x]) [] xs

-- func :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- func f p [] = []
-- func f p xs = map f (filter p xs)

-- map :: (a -> b) -> [a] -> [b]
-- map f [] = []
-- map f xs = foldr (\y ys -> f y:ys) [] xs

-- filter :: (a->Bool) -> [a] -> [a]
-- filter p [] = []
-- filter p xs = foldr (\y ys -> if p y then y:ys else ys) [] xs

-- first :: (a,b) -> a 
-- first (x,_) = x

-- zipSum:: [Int] -> [Int] -> [Int]
-- -- zipSum [] [] = []
-- -- zipSum [] ys = ys
-- -- zipSum xs [] = xs
-- -- zipSum (x:xs) (y:ys) = (x+y): zipSum xs ys

-- zipSum xs ys = zipWith (\x y -> x+y) xs ys

-- unzip :: [(a,b)] -> ([a], [b])
-- unzip xs = (foldr (\(x,y) a -> x : a) [] xs, foldr (\(x,y) a -> y : a) [] xs)

-- f :: [Int] -> Bool
-- f = (>100) . sum . map (\x -> 1 + x * 2) . filter even

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n 

area :: Shape -> Float
area (Circle n) = pi * n^2
area (Rect x y) = x * y
