------------------------------------------------------
-- Functional Programming (Assignment 2)
------------------------------------------------------

-- Name : Pranay Periwal
-- UID  : 3035664697

-- change the module name to your prefered one
module Assignment2 where

------------------------------------------------------
-- Warm up (15 pts)
------------------------------------------------------

data Tree a = Leaf | Branch a (Tree a) (Tree a) deriving (Show, Eq)

foldTree :: b -> (a -> b -> b -> b) -> Tree a -> b
foldTree e _ Leaf = e
foldTree e n (Branch a n1 n2) = n a (foldTree e n n1) (foldTree e n n2)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree Leaf (\x t1 t2 -> Branch (f x) t1 t2)

-- Problem 1 (5 pts)
-- for this problem we let you figure out the type signiture on your own
takeWhileTree :: (a -> Bool) -> Tree a -> Tree a
takeWhileTree p = foldTree Leaf (\x t1 t2 -> if p x
                                                then Branch x t1 t2
                                                else Leaf)

-- Problem 2 (10 pts)
-- for this problem we let you figure out the type signiture on your own
zipTree :: (a -> a -> b) -> Tree a -> Tree a -> Tree b
zipTree f _ Leaf = Leaf
zipTree f Leaf _ = Leaf
zipTree f (Branch val1 subtree11 subtree12) (Branch val2 subtree21 subtree22)
    = Branch (f val1 val2) (zipTree f subtree11 subtree21) (zipTree f subtree12 subtree22)


------------------------------------------------------
-- Propositional Logic (25 pts)
------------------------------------------------------

type Name = String
data Prop = Var Name
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          | Prop :&: Prop
          | Prop :->: Prop
          | Prop :<->: Prop deriving (Eq, Show)

-- Problem 3 (10 pts)

extractProps:: Prop -> [Prop]
extractProps T = [T]
extractProps F = [F]
extractProps (Var s) = [Var s]
extractProps (Not p) = extractProps p
extractProps (p :|: q) = extractProps p ++ extractProps q
extractProps (p :&: q) = extractProps p ++ extractProps q
extractProps (p :->: q) = extractProps p ++ extractProps q
extractProps (p :<->: q) = extractProps p ++ extractProps q

reducedProps :: [Prop] -> [Prop]
reducedProps [] = []
reducedProps (x:xs) = if x `elem` xs then reducedProps xs else x : reducedProps xs

truthOneVar :: Prop -> [(Name, Bool)]
truthOneVar (Var x) = [(x, True), (x, False)]
truthOneVar _ = []

generateTruthTable :: [Prop] -> [Env]
generateTruthTable [] = [[]]
generateTruthTable (x:xs) = [entry:entries | entry <- truthOneVar x, entries <- generateTruthTable xs]

extractPropsAndGenTruthTable :: Prop -> [Env]
extractPropsAndGenTruthTable p = generateTruthTable (reducedProps (extractProps p))

satisfiable :: Prop -> Bool
satisfiable (Var s) = True
satisfiable T = True
satisfiable F = False
satisfiable p = any (\env -> eval env p) (extractPropsAndGenTruthTable p)

unsatisfiable :: Prop -> Bool
unsatisfiable (Var s) = False
unsatisfiable T = False
unsatisfiable F = True
unsatisfiable p = not (all (\env -> eval env p) (extractPropsAndGenTruthTable p))

valid :: Prop -> Bool
valid (Var s) = False
valid T = True
valid F = False
valid p = all (\env -> eval env p) (extractPropsAndGenTruthTable p)

-- Problem 4 (5 pts)
type Env = [(Name, Bool)]

boolForVar :: Env -> Name -> Bool
boolForVar [] s = False
boolForVar ((n,b):xs) s = if n == s then b else boolForVar xs s

eval :: Env -> Prop -> Bool
eval _ T = True
eval _ F = False
eval e (Var x) = boolForVar e x
eval e (Not p) = not (eval e p)
eval e (p :|: q) = eval e p || eval e q
eval e (p :&: q) = eval e p && eval e q
eval e (p :->: q)
                | eval e p && not (eval e q)       = False
                | otherwise                        = True
eval e (p :<->: q)
                | not (eval e p) && not (eval e q) = True
                | eval e p && eval e q             = True
                | otherwise                        = False



-- Problem 5 (10 pts)

extractVars:: Env -> Prop -> [Prop]
extractVars e p = if eval e p
                    then [ if bool then Var name else Not (Var name) | (name, bool) <- e]
                    else []

returnAllDNFLists:: [Env] -> Prop -> [[Prop]]
returnAllDNFLists es p = map (`extractVars` p) es

toDNF :: Prop -> [[Prop]]
toDNF p = filter (/=[]) (returnAllDNFLists (extractPropsAndGenTruthTable p) p)

------------------------------------------------------
-- Trie (30 pts)
------------------------------------------------------


data Trie k v = Trie { value :: Maybe v
                     , subTrees :: [(k, Trie k v)]} deriving (Show)

emptyTrie :: Trie k v
emptyTrie = Trie Nothing []

-- -- Problem 6 (10 pts)
insertTrie :: Eq k => Trie k v -> [k] -> v -> Trie k v
insertTrie (Trie _ subs) [] val = Trie (Just val) subs
insertTrie (Trie v subs) (key:keys) val = Trie v (insert subs) where
    insert [] = [(key, insertTrie emptyTrie keys val)]
    insert ((k, t): subTrie) = if key == k then (k, insertTrie t keys val) : subTrie
                                    else (k, t) : insert subTrie

-- Problem 7 (5 pts)
lookupTrie :: Eq k => [k] -> Trie k v -> Maybe v
lookupTrie [] trie = value trie
lookupTrie (key:keys) (Trie val sub) = case scan key sub of
                                            Nothing -> Nothing
                                            Just subT -> lookupTrie keys subT
                                        where
                                            scan _ [] = Nothing
                                            scan val ((v, subT):rem) =  if v == val then Just subT else scan val rem


-- Problem 8 (5 pts)
fromList :: Eq k => [([k], v)] -> Trie k v
fromList [] = emptyTrie
fromList xs = foldl build emptyTrie xs where
                    build trie (key, val) = insertTrie trie key val

-- Problem 9 (10 pts)
formatString :: String -> [(String, Int)]
formatString "" = []
formatString s = zip (words s) [1..length s]

fromString :: String -> Trie Char Int
fromString s = fromList (formatString s)

------------------------------------------------------
-- Functional Data Structure (30 pts)
------------------------------------------------------

flatten :: Ord a => Tree a -> [a]
flatten Leaf = []
flatten (Branch x l r) = x : merge (flatten l) (flatten r)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- Problem 10 (10 pts)
heapSort :: Ord a => [a] -> [a]
heapSort = flatten . buildHeap

buildHeap :: Ord a => [a] -> Tree a
buildHeap = heapify . buildTree

splitList:: Char -> [a] -> [a]
splitList _ [] = []
splitList 'L' xs = take (length xs `div` 2) xs
splitList 'R' xs = drop (length xs `div` 2) xs
splitList  c  xs = []

buildTree :: Ord a => [a] -> Tree a
buildTree [] = Leaf
buildTree (x:xs) = Branch x (buildTree (splitList 'L' xs)) (buildTree (splitList 'R' xs))

heapify :: Ord a => Tree a -> Tree a
heapify Leaf = Leaf
heapify (Branch x l r) = siftDown x (heapify l) (heapify r)

siftDown :: Ord a => a -> Tree a -> Tree a -> Tree a
siftDown v Leaf Leaf = Branch v Leaf Leaf
siftDown v Leaf (Branch x l r) = if v > x then Branch x Leaf (siftDown v l r) else Branch v Leaf (siftDown x l r)
siftDown v (Branch x l r) Leaf = if v > x then Branch x (siftDown v l r) Leaf else Branch v (siftDown x l r) Leaf
siftDown v (Branch x1 l1 r1) (Branch x2 l2 r2)
                                            | x1 < x2 && v >= x1 = Branch x1 (siftDown v l1 r1) (Branch x2 l2 r2)
                                            | x2 < x1 && v >= x2 = Branch x2 (Branch x1 l1 r1) (siftDown v l2 r2)
                                            | otherwise          = Branch v (Branch x1 l1 r1) (Branch x2 l2 r2)


-- Problem 11 (20 pts)
data PQueue a p = Null
                | Fork Order a p (PQueue a p) (PQueue a p) deriving (Show, Eq)
type Order = Int

sampleTree1 :: PQueue String Int
sampleTree1 = Fork 2 "A" 4 (Fork 1 "B" 5 (Fork 1 "C" 6 Null Null) Null)
                           (Fork 1 "D" 8 Null Null)

sampleTree2 :: PQueue String Int
sampleTree2 = Fork 2 "A" 1 (Fork 2 "B" 3 (Fork 1 "C" 4 Null Null)
                                         (Fork 1 "D" 6 Null Null))
                           (Fork 1 "E" 10 (Fork 1 "F" 11 Null Null) Null)

flattenQ :: Ord p => PQueue a p -> [(a, p)]
flattenQ Null = []
flattenQ (Fork _ val pri Null Null) = [(val, pri)]
flattenQ (Fork _ val pri Null r) = (val, pri): flattenQ r
flattenQ (Fork _ val pri l Null) = (val, pri): flattenQ l
flattenQ (Fork _ val pri (Fork o1 val1 pri2 l1 r1) (Fork o2 val2 pri3 l2 r2)) = (val, pri) :
                                                                                    if pri2 < pri3
                                                                                        then flattenQ (Fork o1 val1 pri2 l1 r1) ++ flattenQ (Fork o2 val2 pri3 l2 r2)
                                                                                        else flattenQ (Fork o2 val2 pri3 l2 r2) ++ flattenQ (Fork o1 val1 pri2 l1 r1)

fork :: a -> p -> PQueue a p -> PQueue a p -> PQueue a p
fork val pri Null Null = Fork 1 val pri Null Null
fork val pri l Null = Fork 1 val pri l Null
fork val pri Null r = Fork 1 val pri Null r
fork val pri (Fork o1 val1 pri1 l1 r1) (Fork o2 val2 pri2 l2 r2) = Fork (1+min o1 o2) val pri (Fork o1 val1 pri1 l1 r1) (Fork o2 val2 pri2 l2 r2)


sampleTree1' :: PQueue String Int
sampleTree1' = fork "A" 4 (fork "B" 5 (fork "C" 6 Null Null) Null)
                          (fork "D" 8 Null Null)

sampleTree2' :: PQueue String Int
sampleTree2' = fork "A" 1 (fork "B" 3 (fork "C" 4 Null Null)
                                      (fork "D" 6 Null Null))
                          (fork "E" 10 (fork "F" 11 Null Null) Null)

checkSiblingLeftNode :: PQueue a p -> PQueue a p
checkSiblingLeftNode (Fork o val pri Null r) = Fork o val pri r Null
checkSiblingLeftNode (Fork o val pri (Fork o1 val1 pri1 l1 r1) (Fork o2 val2 pri2 l2 r2))
                                            = if o1 > o2
                                                then Fork o val pri (Fork o2 val2 pri2 l2 r2) (Fork o1 val1 pri1 l1 r1)
                                                else Fork o val pri (Fork o1 val1 pri1 l1 r1) (Fork o2 val2 pri2 l2 r2)
checkSiblingLeftNode q = q


mergeQ :: Ord p => PQueue a p -> PQueue a p -> PQueue a p
mergeQ Null Null = Null
mergeQ q Null = q
mergeQ Null q = q
mergeQ (Fork o1 val1 pri1 l1 r1) (Fork o2 val2 pri2 l2 r2)
                                        = if pri1 < pri2
                                            then checkSiblingLeftNode (fork val1 pri1 l1 (mergeQ r1 (fork val2 pri2 l2 r2) ))
                                            else checkSiblingLeftNode (fork val2 pri2 l2 (mergeQ r2 (fork val1 pri1 l1 r1)))

insert :: Ord p => a -> p -> PQueue a p -> PQueue a p
insert val pri Null = fork val pri Null Null
insert val pri q = mergeQ (fork val pri Null Null) q


delete :: Ord p => PQueue a p -> ((a, p), PQueue a p)
delete (Fork o val pri l r) = ((val, pri), mergeQ l r)
delete _ = error "Nothing to delete"