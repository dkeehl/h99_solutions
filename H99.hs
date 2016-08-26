-- Ninety-nine Haskell Problems
module H99
    (
      myLast
    , myButLast
    , elementAt
    , myLength
    , myReverse
    , isPalindrome
    , flatten
    , compress
    , pack
    , encode

    , Duplicate(Single, Multiple)
    , encodeModified
    , decodeModified
    , encodeDirect
    , dupli
    , repli
    , dropEvery
    , split
    , slice
    , rotate
    , removeAt

    , insertAt
    , range
    , rnd_select
    , diff_select
    , rnd_permu
    , combinations
    , group27
    , lsort
    , lfsort

    , isPrime
    , myGCD
    , coprime
    , totient
    , primeFactors
    , prime_factors_mult
    , totientA
    , primeR
    , goldbach
    , goldbachList
    
    , table
    , and'
    , or'
    , nand'
    , nor'
    , xor'
    , impl'
    , equ'
    , tablen
    , gray
    , huffman

    , Tree(Empty, Branch)
    , cbalTree
    , symmetric
    , construct
    , symCbalTrees
    , hbalTree
    , hbalTreeNodes

    , countLeaves
    , leaves
    , internals
    , atLevel
    , completeBinaryTree
    , isCompleteBinaryTree
    , layout
    , layout65
    , layout66
    , stringToTree
    , treeToString
    , preorder
    , inorder
    , pre_in_tree
    , tree2ds
    , ds2tree

    , MTree(Node)
    , nnodes
    , treeToString70
    , stringToTree70
    , ipl
    , bottom_up
    , lisp

    , Graph(Graph)
    , Adj(Adj)
    , Friendly(Edge)
    , graphToAdj
    , adjToGraph
    , graphToFriendly
    , friendlyToGraph
    , adjToFriendly
    , friendlyToAdj
    , paths
    , cyclesInGraph
    , s_tree
    , is_tree
    , is_conected
    , prim
    , iso
    , degree
    , sortByDegree
    , kcolor
    , depthfirst
    , connectedcomponents
    , bipartite

    , queens
    , knightsTo
    , closedKnights
    , vonKoch
    , puzzle
    , regular

    , fullWords
    , identifier
    , sudoku
    , nonogram
    , readCrossword
    , solver 
    ) where

import System.Random (randomRIO)
import Control.Monad
import Data.List
import Data.Maybe (maybeToList)
import Data.Char (isLetter, isAlphaNum)

main :: IO ()
main = return ()
-- Promblem 1
-- Find the last element of a list.
myLast :: [a] -> a
myLast [x] = x
myLast (x:x':xs) = myLast (x':xs)

-- Problem 2
-- Find the last but one element of a list.
myButLast :: [a] -> a
myButLast (x: x':[]) = x
myButLast (x:x':x'':xs) = myButLast (x':x'':xs)

-- Problem 3
-- Find the K'th element of a list. The first element
-- in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) k = elementAt xs (k - 1)

-- Problem 4
-- Find the number of elements of a list.
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- Problem 5
-- Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6
-- Find out whether a list is a palindrome.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs

-- Problem 7
-- Flatten a nested list structure.
-- Example (my-flatten '(a (b (c d) e)))
-- (a b c d e)
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten nl = case nl of
    Elem a -> [a]
    List l -> concatMap flatten l

        
-- Problem 8
-- Eliminate consecutive duplicates of list elements.
-- Example:
-- compress "aaaabccaadeeee" = "abcade"
compress :: Eq a => [a] -> [a]
compress xs = case xs of
    []      -> []
    [x]     -> [x]
    x:x':xs -> if x == x'
                then compress (x:xs)
                else x:(compress (x':xs))

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed
-- in separate sublists.
-- Example: pack (a a a a b c c aa d e e e e)
-- ((a a a a) (b) (c c) (a a) (d) (e e e e))
pack :: Eq a => [a] -> [[a]]
pack xs = case xs of
    []   -> []
    [x]  -> [[x]]
    x:xs -> if x == head xs
              then ([x] ++ head (pack xs)):(tail (pack xs))
              else [x]:(pack xs)

-- Problem 10
-- Run-length encoding of a list.
-- Example encode "aaaabccaadeeee"
-- [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')]
encode :: Eq a => [a] -> [(Int, a)]
encode xs = let packedList = pack xs
            in map (\l -> (myLength l, head l)) packedList

-- Problem 11
-- Modified run-length encoding.
-- Only duplicates are transferred as (N E) lists.
data Duplicate a = Single a | Multiple Int a
tuplToDupli :: (Int, a) -> Duplicate a
tuplToDupli (times, ele) = if times == 1
                            then Single ele
                            else Multiple times ele

encodeModified :: Eq a => [a] -> [Duplicate a]
encodeModified =  map tuplToDupli . encode

-- Problem 12
-- Decode a run-length encoded list.
decodeModified :: Eq a => [Duplicate a] -> [a]
decodeModified [] = []
decodeModified (x:xs) =
  case x of
    Single c -> c:decodeModified xs
    Multiple n c -> (replicate n c) ++ decodeModified xs

-- Problem 13
-- Run-length encoding of a list(direct solution).
encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect (x:x':xs) =
  let
    nl = encodeDirect (x':xs) 
    nh = head nl
  in if x == x'
      then case nh of
            Single _ -> (Multiple 2 x):nl
            Multiple n _ -> (Multiple (n + 1) x):nl
      else (Single x):nl

-- Problem 14
-- Duplicate the elements of a list.
dupli l = l >>= \x -> [x, x]

-- Problem 15
-- Replicate the elements of a list a given number of times.
-- Example: repli "abc" 3
-- "aaabbbccc"
repli xs n = xs >>= replicate n

-- Problem 16
-- Drop every N'th element from a list.
-- Example: dropEvery "abcdefghik" 3
-- "acdeghk"
dropEvery xs n = let
                  zippedl = zip [1..] xs
                  flt = \(index, _) -> index `mod` n /= 0
                 in map snd $ filter flt zippedl

-- Problem 17
-- Split a list into two parts; the length of the first
-- part is given.
split [] _ = ([], [])
split xs 0 = ([], xs)
split (x:xs') n = (x:l1, l2)
  where (l1, l2) = split xs' (n - 1)

-- Problem 18
-- Extract a slice from a list.
slice xs m n = snd $ split (take n xs) m

-- Problem 19
-- Rotate a list N places to the left.
rotate xs n =
  let
    (l1, l2) = if n >= 0
                then split xs n
                else split xs (length xs + n)
  in l2 ++ l1

-- Problem 20
-- Remove the K'th element from a list.
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = (x', x:xs')
  where (x', xs') = removeAt (n - 1) xs

-- Problem 21
-- Insert an element at a given position into a list.
insertAt x xs n = l1 ++ (x:l2)
  where (l1, l2) = split xs (n - 1)

-- Problem 22
-- Create a list containing all integers within a given range.
range m n = 
  case compare m n of
    EQ -> [m]
    LT -> m:(range (m + 1) n)
    GT -> m:(range (m - 1) n)

-- Problem 23
-- Extract a given number of randomly selected elements from a list.
rnd_select :: [a] -> Int -> IO [a]
rnd_select _  0 = return []
rnd_select [] _ = return []
rnd_select xs n =
  do r <- randomRIO (1, length xs)
     (x', rest) <- return $ removeAt r xs
     xs' <- rnd_select rest (n - 1)
     return (x':xs')

-- Problem 24
-- Draw N different random numbers from the set 1..M.
diff_select n m = rnd_select (range 1 m) n

-- Problem 25
-- Generate a random permutation of the elements of a list.
rnd_permu xs = rnd_select xs $ length xs

-- Problem 26
-- Generate the combinations of K distinct objects chosen from
-- the N elements of a list.
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = [[]]
combinations n (x:xs) =
  if n == length (x:xs)
    then [x:xs]
    else combinations n xs ++ (map (x:) $ combinations (n - 1) xs)

-- Problem 27
-- Group the elements of a set into disjoint subsets.
group27 :: Eq a => [Int] -> [a] -> [[[a]]]
group27 policy people
  | sum policy == length people = g policy people
  | otherwise = []
  where
    g [] _ = []
    g [x] l = [[l]]
    g (x:xs') l = do
      xl <- combinations x l
      remain <- g xs' (l \\ xl)
      return (xl:remain)

-- Problem 28
-- Sorting a list according to length of sublists.
sortPairList :: Ord b => [([a], b)] -> [([a], b)]
sortPairList [] = []
sortPairList [p] = [p]
sortPairList ps = merge l1' l2'
  where (l1, l2) = split ps (length ps `div` 2)
        l1' = sortPairList l1
        l2' = sortPairList l2
        merge [] [] = []
        merge [y] [] = [y]
        merge [] [y] = [y]
        merge ((y1, i1):ys1) ((y2, i2):ys2) =
          if i1 <= i2
            then (y1, i1):(merge ys1 ((y2, i2):ys2))
            else (y2, i2):(merge ((y1, i1):ys1) ys2)

lsort l = sortPairList $ zip l (map length l)
lfsort l =
    sortPairList $ zip l freq
  where
    lenList = map length l
    freq = map (\x -> length (filter (\y -> y == x) lenList)) lenList
-- Problem 31
-- Determine whether a given integer number is prime.
isPrime :: Int -> Bool
isPrime 2 = True
isPrime n = null [a | a <- [2..n - 1], n `mod` a == 0]

-- Problem 32
-- Determine the greatest common divisor of two positive integer
-- numbers. Use Euclid'd algorithm.
myGCD x y = if y == 0
              then x
              else myGCD y (x `mod` y)

-- Problem 33
-- Determine whether two positive integer numbers are coprime.
-- Two numbers are coprime if their greatest common divisor equals 1.
coprime m n = myGCD m n == 1

-- Problem 34
-- Calculate the Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as the number
-- of positive integers r (1 <= r < m) that ar coprime to m.
totient 1 = 1
totient n = length $ filter (coprime n) [1..n - 1]

-- Problem 35
-- Determine the prime factors of a given positive integer.
-- Construct a flat list containing the prime factors in ascending order.
firstFactor :: Int -> Int -> [Int]
firstFactor n m
  | m > n = []
  | n `mod` m == 0 = [m]
  | otherwise = firstFactor n (m + 1)

primeFactors :: Int -> [Int]
primeFactors n = let [x] = firstFactor n 2
                 in  x:(primeFactors (n `div` x))

-- Problem 36
-- Calculate the prime factors of a given positive integer.
prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n = zip l1 l2
  where l1 = primeFactors n
        count x xs = case xs of
                    [] -> 0
                    x':xs' -> if x == x'
                               then 1 + (count x xs')
                               else count x xs' 
        l2 = map (\x -> count x l1) l1

-- Problem 37
-- Calculate Euler's totient function phi(m) (improved).
totientA n = g xs
  where xs = prime_factors_mult n
        g ((p, m):l) = (p - 1) * p ^ (m - 1) * g l

-- Problem 39
-- A list of prime numbers.
primeR :: Int -> Int -> [Int]
primeR n m = filter isPrime $ range n m

-- Problem 40
-- Goldbach's cojecture.
goldbach :: Int -> (Int, Int)
goldbach n =
  head [(x, y) | x <- [2..n], y <- [2..n], isPrime x, isPrime y, x + y == n]   

-- Problem 41
-- A
-- Given a range of integers by its lower and upper limit, print a list of
-- all even numbers and their Goldbach composition.
-- B
-- Find out how many numbers of which the primes are both bigger than say 50.
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList m n = map goldbach even_list
  where
    even_list = filter even (range m n)

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' m n k = filter (\(x, y) -> x > k) (goldbachList m n)

-- Problem 46
-- Write a predicate table/3 which prints the truth table of a given
-- logical expression in two variables.
table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn
         [show a ++ " " ++ show b ++ " " ++ show (f a b)
         | a <- [True, False], b <- [True, False]]

and' :: Bool -> Bool -> Bool
and' a b = case a of
              False -> False
              True -> b

or' :: Bool -> Bool -> Bool
or' a b = case a of
              True -> True
              False -> a

nand' :: Bool -> Bool -> Bool
nand' a b = not $ and' a b

nor' :: Bool -> Bool -> Bool
nor' a b = not $ or' a b

xor' :: Bool -> Bool -> Bool
xor' = (/=)

impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _ _ = True

equ' :: Bool -> Bool -> Bool
equ' = (==)

-- Problem 47
-- Continue problem P46 by defining and', or' etc as being operators.
-- Define operator precedence as usual.
infixl 7 `equ'`
infixl 6 `and'`
infixl 5 `xor'`
infixl 4 `or'`

-- Problem 48
-- Generalize problem 47 in such a way that the logical expression may
-- contain any number of logical variables.
boolCombination :: Int -> [[Bool]]
boolCombination 0 = [] 
boolCombination n = (++) <$> [[True], [False]] <*> boolCombination (n - 1)

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn l
  where
    l = map (\(bool_list, fb) -> concatMap (++ " ") bool_list ++ fb) l_to_str
    l_to_str = map (\(list, bool) -> (map show list, show bool)) l_orig
    l_orig = map (\bl -> (bl, f bl)) (boolCombination n)

-- Problem 49
-- Gray codes.
-- An n-bit Gray code is a sequence of n-bit strings constructed according to 
-- certain rules. For example:
-- n = 1: C(1) = ['0', '1'].
-- n = 2: C(2) = ['00', '01', '11', '10']. 
-- n = 3: C(3) = ['000', '001', '011', '010', '110', '111', '101', '100'].
gray :: Int -> [String]
gray 0 = []
gray n = (++) <$> ["0", "1"] <*> gray (n - 1)

-- Problem 50
-- Huffman codes.
data HTree a = HLeaf a | HBranch (HTree a) (HTree a)

huffmanTree :: [(Int, HTree Char)] -> HTree Char
huffmanTree [(_, t)] = t
huffmanTree l = let (w1, t1):(w2, t2):l' = sortBy compare_first l
                    compare_first (x1, _) (x2, _) = compare x1 x2
                in huffmanTree $
                    insertBy compare_first (w1 + w2, HBranch t1 t2) l'

decodeHTree :: HTree a -> [(a, String)]
decodeHTree (HLeaf x) = [(x, "")]
decodeHTree (HBranch l r) = [(x, '0':code) | (x, code) <- decodeHTree l] ++
                            [(x, '1':code) | (x, code) <- decodeHTree r]

listToHTree :: [(Char, Int)] -> [(Int, HTree Char)]
listToHTree = map (\(x, w) -> (w, HLeaf x))

huffman :: [(Char, Int)] -> [(Char, String)]
huffman = decodeHTree . huffmanTree . listToHTree

-- Problem 55
-- Construct completely balanced binary trees.
data Tree a = Empty
            | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let n1 = (n - 1) `div` 2
                 n2 = n - 1 - n1
             in  [Branch 'x' l r | i <- [n1..n2],
                                   l <- cbalTree i,
                                   r <- cbalTree (n - 1 - i)]

-- Problem 56
-- Symmetric binary trees
-- Let us call a binary tree symmetric if you can draw a vertical line through 
-- the root node and then the right subtree is the mirror image of the left
-- subtree. Write a predicate symmetric to check whether a given binary tree
-- is symmetric. 
mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror Empty _ = False
mirror _ Empty = False
mirror (Branch _ l1 r1) (Branch _ l2 r2) = mirror l1 r2 && mirror l2 r1

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

-- Problem 57
-- Binary search trees.(dictionaries)
-- Write a predicate to construct a binary search tree from a list of integers.
construct :: [Int] -> Tree Int
construct [] = Empty
construct (x:xs) = Branch x (construct $ filter (< x) xs) 
                            (construct $ filter (> x) xs)

-- Problem 58
-- construct all symmetric, completely balanced binary trees with a given
-- number of nodes.
symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree

-- Problem 59
-- Construct height-balanced binary trees.
-- In a height balanced binary tree, the following property holds for every
-- node: The height of its left sutree and the heigth of its right subtree are
-- almost equal, which means their difference is not greater than one.
hbalTree' :: Char -> Int -> [Tree Char]
hbalTree' _ 0 = [Empty]
hbalTree' x 1 = [leaf x]
hbalTree' x n = [Branch x l r | i <- [(n - 2), (n - 1)],
                                j <- [(n - 2), (n - 1)], 
                                i + j /= (n - 2) * 2, 
                                l <- hbalTree i,
                                r <- hbalTree j]
hbalTree :: Int -> [Tree Char]
hbalTree = hbalTree' 'x'


-- Problem 60
-- Construct height-balanced binary trees with a given number of nodes.
minNodes :: Int -> Int
minNodes 0 = 0
minNodes 1 = 1
minNodes h = 1 + fibs !! h

fibs :: [Int]
fibs = 0:1:zipWith (+) fibs (tail fibs)

maxHeight :: Int -> Int
maxHeight n = h
  where
    h : _ = [x | x <- [0..],
                 minNodes x <= n,
                 minNodes (x + 1) > n]

minHeight :: Int -> Int
minHeight n = ceiling $ logBase 2 $ fromIntegral (n + 1)

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Branch _ l r) = 1 + countNodes l + countNodes r

hbalTreeNodes :: Char -> Int -> [Tree Char]
hbalTreeNodes _ 0 = [Empty]
hbalTreeNodes x 1 = [leaf x]
hbalTreeNodes x n = [ts | i <- [(minHeight n)..(maxHeight n)]
                        , ts <- filter ((== n) . countNodes) (hbalTree' x i)]

-- Problem 61
-- Count the leaves of a binary tree.
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

-- Problem 61A
-- Collect the leaves of a binary tree in a list.
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x l r) = x : (leaves l ++ leaves r)

-- Problem 62
-- Collect the internal nodes of a binary tree in a list.
-- An internal node of a binary tree has either one or two non-empty successors.
internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch x l r) = x : (internals l ++ internals r)

-- Problem 62B
-- Collect the nodes at a given level in a list.
-- A node of a binary tree is at level N if the path from the root to the node
-- has length N - 1. The root node is at level 1.
atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel _ 0 = []
atLevel (Branch x _ _) 1 = [x]
atLevel (Branch _ l r) n = atLevel l (n - 1) ++ atLevel r (n - 1)

-- Problem 63
-- Construct a complete binary tree with n nodes.
-- A complete binary tree with height H is defined as follows:
-- * The levels 1,2,3..,H-1 contain the maximum number of nodes
-- * At level H, which may contain less than the maximum possible number of nodes.
--   all the nodes are "left-adjusted".
completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = generate_tree 1
  where
    generate_tree x
      | x > n = Empty
      | otherwise = Branch 'x' (generate_tree (2 * x))
                               (generate_tree (2 * x + 1))

isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree t = (max_leaves_index t 1) == (countLeaves t)

max_leaves_index :: Tree a -> Int -> Int
max_leaves_index Empty _ = 0
max_leaves_index (Branch _ Empty Empty) n = n
max_leaves_index (Branch _ l r) n =
    max (max_leaves_index l (2 * n)) (max_leaves_index r (2 * n + 1))

-- Problem 64
-- Determine the position of each node in a rectangle grid.
-- v(x) is equal to the position of the node w in the inorder sequence.
-- v(y) is equal to the depth of the node v in the tree.
layout' :: (Int, Int) -> Tree a -> Tree (a, (Int, Int))
layout' _ Empty = Empty
layout' (x0, y0) (Branch node l r) =
    Branch (node, (x0 + x, y0)) (layout' (x0, y0 + 1) l)
                                (layout' (x0 + x + 1, y0 + 1) r)
  where
    x = countNodes l


layout :: Tree a -> Tree (a, (Int, Int))
layout = layout' (1, 1)

-- Problem 65
layout65' :: (Int, Int) -> Tree a -> Tree (a, (Int, Int))
layout65' _ Empty = Empty
layout65' (x0, y0) t@(Branch node l r) =
    Branch (node, (x0, y0)) (layout65' (x0 - 2 ^ (h - y0 - 1), y0 + 1) l)
                            (layout65' (x0 + 2 ^ (h - y0 - 1), y0 + 1) r)
  where
    h = height t

height :: Tree a -> Int
height Empty = 0
height (Branch _ l r) = 1 + maximum [height l, height r]

type TreeGrid a = Tree (a, (Int, Int))

moveTree :: (Int, Int) -> TreeGrid a -> TreeGrid a
moveTree _ Empty = Empty
moveTree (delta_x, delta_y) (Branch (node, (x, y)) l r) =
    Branch (node, (x + delta_x, y + delta_y)) (moveTree (delta_x, delta_y) l)
                                              (moveTree (delta_x, delta_y) r)

layout65 t = moveTree (to_right, 0) (layout65' (0, 0) t)
  where
    to_right = left_margin t * (-1)

left_margin :: TreeGrid a -> Int
left_margin Empty = 0
left_margin (Branch (_, (x, _)) Empty _) = x
left_margin (Branch _ l _) = left_margin l

-- Problem 66
layout66' :: (Int, Int) -> Tree a -> TreeGrid a
layout66' _ Empty = Empty
layout66' (x0, y0) (Branch node l r) =
    Branch (node, (x0, y0)) (moveTree (- n , 0) l_grid)
                            (moveTree (n, 0) r_grid)
  where
    n = if d >= 0 then d `div` 2 + 1 else 0
    d = maximum $ zipWith (\(x1, _) (x2, _) -> x1 - x2) rs ls
    l_grid = layout66' (x0 - 1, y0 + 1) l
    r_grid = layout66' (x0 + 1, y0 + 1) r
    rs = most_rigtht l_grid
    ls = most_left r_grid

most_left :: TreeGrid a -> [(Int, Int)]
most_left t = map (minimumBy (\(x1, _) (x2, _) -> compare x1 x2))  l
  where
    l = group_by_y all_axis
    all_axis = axises t

most_rigtht :: TreeGrid a -> [(Int, Int)]
most_rigtht t = map (maximumBy (\(x1, _) (x2, _) -> compare x1 x2)) l
  where
    l = group_by_y all_axis
    all_axis = axises t

axises :: TreeGrid a -> [(Int, Int)]
axises Empty = []
axises (Branch (_, axis) l r) = axis : (axises l ++ axises r)

group_by_y :: [(Int, Int)] -> [[(Int, Int)]]
group_by_y = groupBy (\(_, y1) (_, y2) -> y1 == y2)

layout66 :: Tree a -> TreeGrid a
layout66 t = moveTree (to_right, 0) t'
  where
    to_right = f_min l * (-1)
    t' = layout66' (0, 0) t
    l = axises t'
    f_min = minimum . fst . unzip

-- Problem 67A
-- A string representation of binary tree.
treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch x l r) =
    x : ("(" ++ treeToString l ++ "," ++ treeToString r ++ ")")

stringToTree' :: String -> (Tree Char, String)
stringToTree' "" = (Empty, "")
stringToTree' [x] = (leaf x, "")
stringToTree' (x:x':xs)
  | x == '(' || x == ',' = stringToTree' (x':xs)
  | x' == ',' || x' == ')' = (leaf x, xs)
  | otherwise = (Branch x l r, remain)
  where
    (l, xs') = stringToTree' (x':xs)
    (r, remain) = stringToTree' xs' 

stringToTree :: String -> Tree Char
stringToTree = fst . stringToTree'

-- Problem 68
-- a) Write predicates preorder and inorder that construct the preorder and
--    inorder sequence of a given binary tree.
-- b) If both the preorder sequence and the inorder sequence of the nodes of a
--    binary tree are given, then the tree is determined unambiguously. Write
--    a predicate pre_in_tree that does the job.
preorder :: Tree Char -> String
preorder Empty = ""
preorder (Branch x l r) = x : (preorder l ++ preorder l)

inorder :: Tree Char -> String
inorder Empty = ""
inorder (Branch x l r) = inorder l ++ [x] ++ inorder r

pre_in_tree :: String -> String -> Tree Char
pre_in_tree "" "" = Empty
pre_in_tree po@(x:xs) io = Branch x (pre_in_tree pol iol) (pre_in_tree por ior)
  where
    iol = takeWhile (/= x) io
    (pol, por) = splitAt (length iol) xs
    ior = drop (length iol + 1) io

-- Problem 69
-- Dotstring representation of binary trees.
-- A tree can be represented by the preorder sequence of its nodes in which
-- dots (.) are inserted where an empty subtree is encountered during the tree
-- tranversal.
tree2ds :: Tree Char -> String
tree2ds Empty = "."
tree2ds (Branch x l r) = x : (tree2ds l ++ tree2ds r)

ds2tree :: String -> (Tree Char, String)
ds2tree "" = (Empty, "")
ds2tree (x:xs)
  | x == '.'  = (Empty, xs)
  | otherwise = (Branch x l r, xs'')
  where
    (l, xs')  = ds2tree xs
    (r, xs'') = ds2tree xs'

-- Problem 70C
-- Count the nodes of a multiway tree.
data MTree a = Node a [MTree a]
    deriving (Eq, Show)

nnodes :: MTree a -> Int
nnodes (Node _ []) = 1
nnodes (Node _ l) = 1 + (sum $ map nnodes l)

-- Problem 70
-- Tree construction from a node string.
treeToString70 :: MTree Char -> String
treeToString70 (Node x []) = [x] 
treeToString70 (Node x l) =
    x : (foldr (\y acc -> treeToString70 y ++ "^" ++ acc) "^" l)

stringToTree70 :: String -> MTree Char
stringToTree70 (x:xs) = Node x (fst (stringToTrees xs))
  where
    stringToTrees [] = ([], "")
    stringToTrees (y:ys)
      | y == '^'  = ([], xs)
      | otherwise = ([Node y trees1] ++ trees2, rest)
          where
            (trees1, rest1) = stringToTrees ys
            (trees2, rest)  = stringToTrees rest1

-- Problem 71
-- Determine the internal path length of a tree.
ipl :: MTree a -> Int
ipl (Node _ []) = 0
ipl (Node _ l) = length l + (sum $ map ipl l)

-- Problem 72
-- Construct the bottom-up order sequence of the tree nodes.
bottom_up :: MTree Char -> String
bottom_up (Node x []) = [x]
bottom_up (Node x l) = (concat $ map bottom_up l) ++ [x]

-- Problem 73
-- List-like tree representation.
lisp :: MTree Char -> String
lisp (Node x []) = [x]
lisp (Node x l) = "(" ++ [x] ++ " " ++ s ++ ")"
  where
    s = intercalate " " $ map lisp l

-- Problem 80
-- Write predicates to convert between the different graph representations.
data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)
data Adj a = Adj [(a, [a])] deriving (Show, Eq)
data Friendly a = Edge [(a, a)] deriving (Show, Eq)

graphToAdj :: Eq a => Graph a -> Adj a
graphToAdj (Graph [] _) = Adj []
graphToAdj (Graph (n:ns) edges) = Adj ((n, en) : l)
  where
    en = foldr f [] edges
    f (x, y) acc
      | x == n    = y : acc
      | y == n    = x : acc
      | otherwise = acc
    Adj l = graphToAdj (Graph ns edges)

adjToGraph :: Eq a => Adj a -> Graph a
adjToGraph (Adj l) = Graph ns (nubBy same_edge es)
  where
    ns = map fst l
    es = [(n, n') | (n, adjs) <- l, n' <- adjs]
    same_edge (x1, y1) (x2, y2) = x1 == x2 && y1 == y2 || x1 == y2 && x2 == y1

graphToFriendly :: Eq a => Graph a -> Friendly a
graphToFriendly (Graph [] _) = Edge []
graphToFriendly (Graph ns es) = Edge (es ++ iso_nodes)
  where
    iso_nodes = do
        n <- ns
        let (ns1, ns2) = unzip es
        guard $ n `notElem` (ns1 ++ ns2)
        return (n, n)

friendlyToGraph :: Eq a => Friendly a -> Graph a
friendlyToGraph (Edge es) = Graph ns (filter (\(x, y) -> x /= y) es)
  where
    (ns1, ns2) = unzip es
    ns = nub (ns1 ++ ns2)

adjToFriendly :: Eq a => Adj a -> Friendly a
adjToFriendly = graphToFriendly . adjToGraph

friendlyToAdj :: Eq a => Friendly a -> Adj a
friendlyToAdj = graphToAdj . friendlyToGraph

-- Problem 81
-- Path from one node to another one.
-- Write a function that, given two nodes a and b in a graph, returns all
-- the acyclic from a to b.
paths' :: Eq a => [a] -> a -> a -> [(a, a)] -> [[a]]
paths' trail source dest edges
  | source == dest = [source:trail]
  | otherwise = do
        let (nexts, rest) = partition ((== source) . fst) edges
        next <- nexts
        paths' (source:trail) (snd next) dest rest

paths :: Eq a => a -> a -> [(a, a)] -> [[a]]
paths source dest edges = map reverse $ paths' [] source dest edges

-- Problem 82
-- Cycle from a given node.
cyclesInGraph :: Eq a => a -> [(a, a)] -> [[a]]
cyclesInGraph n edges = map (\l -> [n] ++ l ++ [n]) mid_paths
  where
    srcs = filter ((== n) . fst) edges
    dests = filter ((== n) . snd) edges
    mid_paths = do
        (src, _)  <- srcs
        (_, dest) <- dests
        paths src dest edges

-- Problem 83
-- Construct all spanning trees.
s_tree :: Eq a => Adj a -> [Adj a]
s_tree (Adj l) = do
    (x, _) <- l
    s_tree' x (Adj l)

s_tree' :: Eq a => a -> Adj a -> [Adj a]
s_tree' _ (Adj []) = []
s_tree' x (Adj l) = do
    let (next, rest) = partition (\(v, _) -> v == x) l
    (x, es) <- next
    e <- es
    Adj ts <- s_tree' e (Adj rest)
    return $ Adj ((x, [e]):ts)

is_tree :: Eq a => Adj a -> Bool
is_tree g = g `elem` s_tree g

is_conected :: Eq a => Adj a -> Bool
is_conected g = vertices g == (maximum $ map vertices (s_tree g))
  where
    vertices (Adj l) = length l

-- Problem 84
-- Construct the minimal spanning tree.
data Adj1 a = Adj1 [(a, [(a, Int)])] deriving (Eq, Show)

prim' :: Eq a => Adj1 a -> Adj1 a -> Adj1 a
prim' t (Adj1 []) = t
prim' (Adj1 vs) (Adj1 vs') = prim' (Adj1 tr) (Adj1 rest)
  where
    tr = extent vs safe_edge
    rest = filter (\(v, _) -> v /= v1) vs'
    extent l (v1, v2, d) = 
      (v2, [(v1, d)]):(map (\(v, vs) -> if v == v1 then (v, (v2,d):vs) else (v, vs)) l)
    safe_edge@(v1, v2, d) = minimumBy compare_the_third common_edges
    common_edges = [(v1, v2, d) |
                      (v1, _) <- vs,
                      (v2, vl) <- vs',
                      (v', d) <- vl,
                      v1 == v']
    compare_the_third (_, _, x1) (_, _, x2) = compare x1 x2

prim :: Eq a => Adj1 a -> Adj1 a
prim (Adj1 []) = Adj1 []
prim (Adj1 ((s, _):vl)) = prim' (Adj1 [(s, [])]) (Adj1 vl)

-- kruskal :: Adjl a -> Adjl a

-- Problem 85
-- Graph isomophism

-- First generate all the bijective functions from a list to the other.
biFuncs :: [a] -> [b] -> [[(a, b)]]
biFuncs l1 l2
  | length l1 /= length l2 = []
  | otherwise = do
        ys <- permutations l2
        return $ zip l1 ys

-- Then transform the edges.
transEdge :: (Eq a, Eq b) => [(a, b)] -> [(a, a)] -> [(b, b)]
transEdge fs es = map f es
  where
    f (x0, y0) = (x1, y1)
      where
        x1 = search_a x0 fs
        y1 = search_a y0 fs
        search_a x ps = snd $ head $ filter (\(m, _) -> m == x) ps


iso :: (Eq a, Eq b) => Graph a -> Graph b -> Bool
iso (Graph vs1 es1) (Graph vs2 es2) = not $ null isomophism
  where
    isomophism = [func | func <- biFuncs vs1 vs2,
                         let es2' = transEdge func es1,
                         es2 \\ es2' == [] ]

-- Problem 86
-- Node degree and graph coloration.
-- a. Write a predicate that determines the degree of a given node.
-- b. Write a predicate that generates a list of all nodes of a graph sorted
-- according to decreasing degree.
-- c. Use Welch-Powell's algorithm to paint the nodes of a graph in such a
-- way that adjacent nodes have different colors.

degree :: Eq a => Adj a -> a -> Maybe Int
degree (Adj l) x = fmap length $ lookup x l

sortByDegree :: Eq a => Adj a -> [a]
sortByDegree (Adj l) = map fst (sortBy f l)
  where
    f (v1, vs1) (v2, vs2) = compare (length vs2) (length vs1)

kcolor' :: Eq a => [a] -> [Int] -> [(a, Int)] -> Adj a -> [(a, Int)]
kcolor' [] _ colored _ = colored
kcolor' (v:vs) (clr:clrs) colored g@(Adj l) = kcolor' vs' clrs (clr_vs ++ colored) g
  where
    clr_vs = (v, clr):adj_nodes_c
    adj_nodes_c = zip adj_nodes $ repeat clr
    adj_nodes = [n | (x, xs) <- l, x == v, n <- xs]
    vs' = vs \\ adj_nodes

kcolor :: Eq a => Adj a -> [(a, Int)]
kcolor g = kcolor' (sortByDegree g) [1..] [] g

-- Problem 87
-- Depth-first order graph traversal.
depthfirst' :: Eq a => Graph a -> [a] -> [a]
depthfirst' _ [] = []
depthfirst' g@(Graph vs es) (top:stack)
  | filter (== top) vs == [] = depthfirst' g stack
  | otherwise = top:(depthfirst' (Graph not_found es) search_list)
  where
    not_found = filter (/= top) vs
    search_list = adj_nodes ++ stack
    adj_nodes = es >>= (\(x, y) -> if x == top 
                                    then [y]
                                    else if y == top then [x] else []) 

depthfirst :: Eq a => Graph a -> a -> [a]
depthfirst g n = depthfirst' g [n]

-- Problem 88
-- Connected components.
connectedcomponents :: Eq a => Graph a -> [[a]]
connectedcomponents (Graph [] _) = []
connectedcomponents g@(Graph (v:vs) es) =
    vs':connectedcomponents (Graph remain es)
  where
    vs' = depthfirst g v
    remain = vs \\ vs'

-- Problem 89
-- Bipartite graphs.
-- Write a predicate that finds out whether a given graph is bipartite.
bipartite0 :: Eq a => Adj a -> [a] -> [a] -> [a] -> Bool 
bipartite0 _ red blue [] = True 
bipartite0 g@(Adj l) red blue stack
  | red `intersect` blue /= [] = False 
  | otherwise = bipartite1 g (stack ++ red) blue stack'
  where
    stack' = [x | r <- stack,
                  (v, adj_nodes) <- l,
                  v == r,
                  x <- adj_nodes,
                  notElem x blue]

bipartite1 :: Eq a => Adj a -> [a] -> [a] -> [a] -> Bool
bipartite1 _ red blue [] = True 
bipartite1 g@(Adj l) red blue stack
  | red `intersect` blue /= [] = False 
  | otherwise = bipartite0 g red (stack ++ blue) stack'
  where
    stack' = [x | r <- stack,
                  (v, adj_nodes) <- l,
                  v == r,
                  x <- adj_nodes,
                  notElem x red]

bipartite :: Eq a => Adj a -> Bool
bipartite (Adj []) = False
bipartite g@(Adj (x:xs)) = bipartite0 g [] [] [fst x]

-- Problem 90
-- Eight queens problem.
queens :: Int -> [[Int]]
queens n = filter none_diagonal $ permutations [1..n]
  where
    none_diagonal l = null (diagonal_pairs $ zip [1..n] l)
    diagonal_pairs ps = [((x1, y1), (x2, y2)) | (x1, y1) <- ps,
                                                (x2, y2) <- ps,
                                                x1 /= x2,
                                                abs (x1 - x2) == abs (y1 - y2)]
-- Problem 91
-- Knight's tour
type Square = (Int, Int)
backTracing :: Int -> [Square] -> [[Square]]
backTracing margin trace@(current:_) = [next : trace | next <- possible_moves]
  where
    possible_moves =
        filter (\pos -> on_board pos && notElem pos trace) (moves_of current)
    moves_of (x, y) =
        [(x + dx, y + dy) | (dx, dy) <- [(1, 2), (1, -2), (-1, 2), (-1, -2),
                                         (2, 1), (2, -1), (-2, 1), (-2, -1)]]
    on_board (x, y) = x > 0 && x <= margin && y >0 && y <= margin

knightsTo :: Int -> Square -> [[Square]]
knightsTo size pos = n_steps_back_tracing (size ^ 2 - 1)
  where
    n_steps_back_tracing 0 = [[pos]]
    n_steps_back_tracing n = do
        trace <- n_steps_back_tracing (n - 1)
        backTracing size trace


closedKnights :: Int -> [[Square]]
closedKnights size = map reverse $ knightsTo size (1, 1)

-- Problem 92
-- Von Koch's conjecture
vonKoch :: [(Int, Int)] -> [[Int]]
vonKoch edges = do
    let n = length edges + 1
    nodes <- permutations [1..n]
    let edges' = map (\(x, y) -> abs (nodes !! (x - 1) - nodes !! (y - 1))) edges
    guard $ nub edges' == edges'
    return nodes

-- Problem 93
-- An arithmetic puzzle.
-- Given a list of integer numbers, find a correct way of inserting
-- arithmetic signs (operators) such that the result is a correct
-- equation.
type Equation = (Expr, Expr)
data Expr = Const Int
          | Binary Expr Op Expr
          deriving (Eq)
data Op = Plus | Minus | Multiply | Divide
          deriving (Bounded, Eq, Enum, Show)
type Value = Rational

-- generate all correct equations
equations :: [Int] -> [Equation]
equations [] = error "Empty list of numbers"
equations [x] = error "Only one number"
equations xs = do
    (xs1, xs2) <- splits xs
    (e1, v1)   <- exprs xs1
    (e2, v2)   <- exprs xs2
    guard $ v1 == v2
    return (e1, e2)

-- generate all equations from a list of number.
exprs :: [Int] -> [(Expr, Value)]
exprs [x] = [(Const x, fromIntegral x)]
exprs xs = do
    (xs1, xs2) <- splits xs
    (e1, v1)   <- exprs xs1
    (e2, v2)   <- exprs xs2
    op <- [minBound..maxBound]
    guard $ not (associative op e2)
    v <- maybeToList (apply op v1 v2)
    return (Binary e1 op e2, v)

-- split a list into two lists.
splits :: [a] -> [([a], [a])]
splits xs = do
    let n = length xs - 1
    m <- [1..n]
    return $ splitAt m xs

associative :: Op -> Expr -> Bool
associative Plus (Binary _ Plus _) = True
associative Multiply (Binary _ Multiply _) = True
associative _ _ = False

apply :: Op -> Value -> Value -> Maybe Value
apply Plus x y = Just (x + y)
apply Minus x y = Just (x - y)
apply Multiply x y = Just (x * y)
apply Divide x 0 = Nothing
apply Divide x y = Just (x / y)

show_equotion :: Equation -> String
show_equotion (e1, e2) = showsPrec 0 e1 . showString " = " . showsPrec 0 e2 $ ""

instance Show Expr where
  showsPrec _ (Const n) = shows n
  showsPrec p (Binary e1 op e2) = showParen (op_prec < p) $
      showsPrec op_prec e1 . showString op_sym . showsPrec (op_prec + 1) e2
    where
      op_prec = precedence op
      op_sym = opSymbol op

precedence :: Op -> Int
precedence Plus = 1
precedence Minus = 1
precedence Multiply = 2
precedence Divide = 2

opSymbol :: Op -> String
opSymbol Plus = "+"
opSymbol Minus = "-"
opSymbol Multiply = "*"
opSymbol Divide = "/"

puzzle :: [Int] -> [String]
puzzle xs = map show_equotion (equations xs)

-- Problem 94
-- Generate K-regular simple graphs with N nodes.
-- In a K-regular simple graph all nodes have a degree of K, i.e. the number
-- of edges incident in a node is K.
regular :: Int -> Int -> [Graph Int]
regular n k
  | k >= n || odd k && odd n = []
  | otherwise = nubBy iso (filter isRegular $ generateGraph n (n * k `div` 2))
-- generate all graphs with N nodes and M edges.
generateGraph :: Int -> Int -> [Graph Int]
generateGraph n m = [Graph nodes edges |
                     let nodes = [1..n],
                     edges <- combinations m $ generateEdges nodes]
 
generateEdges :: [a] -> [(a, a)]
generateEdges [] = []
generateEdges (x:xs) = zip (repeat x) xs ++ generateEdges xs

-- find whether a graph is regular.
isRegular :: Eq a => Graph a -> Bool
isRegular g = all_equal $ map (length . snd) l
  where
    Adj l = graphToAdj g
    all_equal xs = case xs of
        []       -> True
        [x]      -> True
        x:x':xs' -> (x == x') && all_equal (x':xs') 

-- Problem 95
-- English number words.
toFullWord :: Char -> String
toFullWord n = case n of
  '1' -> "one"
  '2' -> "two"
  '3' -> "tree"
  '4' -> "four"
  '5' -> "five"
  '6' -> "six"
  '7' -> "seven"
  '8' -> "eight"
  '9' -> "nine"
  '0' -> "zero"

fullWords :: Int -> String
fullWords  = concat . intersperse "-" . map toFullWord . show 

-- Problem 96
-- Syntax checker.
--
-- identifier
-- ------->letter---->------------------>--------------------->------>
--                   |                                       |
--                   ->---------------->----->letter--------->
--                   |    |           |  |                   |
--                   |    ->---"-"---->  ->--->digit--------->
--                   |                                       |
--                   <-------------------<------------------<-
identifier :: String -> Bool
identifier [] = False
identifier (c:cs)
  | isLetter c = check cs
  | otherwise = False
  where
    check [] = True
    check (x:xs)
      | isLetter x || isAlphaNum x = check xs
      | x == '-' = case xs of
            [] -> False
            y:ys -> (isLetter x || isAlphaNum x) && check ys
      | otherwise = False

-- Problem 97
-- Sudoku
type Spot = ((Int, Int), Int)

sudoku' :: [Spot] -> [Spot] -> [([Spot], [Spot])]
sudoku' [] checked = [([], checked)]
sudoku' (((x, y), 0):unchecked) checked = do
    n <- [1..9]
    let s = ((x, y), n)
    guard $ fit s unchecked && fit s checked
    p <- sudoku' unchecked (s:checked)
    return p
sudoku' (s:unchecked) checked = sudoku' unchecked (s:checked)

fit :: Spot -> [Spot] -> Bool
fit ((x, y), v) l = notInRow && notInColumn && notInSquare
  where
    notInRow = null [v' | ((x', y'), v') <- l, x == x', v == v']
    notInColumn = null [v' | ((x', y'), v') <- l, y == y', v == v']
    notInSquare =
        null [v' | ((x', y'), v') <- l, f x == f x', f y == f y', v == v']
      where
        f n = (n - 1) `div` 3

sudoku :: [Spot] ->[[Spot]]
sudoku = map snd . flip sudoku' []

-- Problem 98
-- Nonograms.
--
-- Problem statement:          Solution:
-- |_|_|_|_|_|_|_|_| 3         |_|X|X|X|_|_|_|_| 3           
-- |_|_|_|_|_|_|_|_| 2 1       |X|X|_|X|_|_|_|_| 2 1         
-- |_|_|_|_|_|_|_|_| 3 2       |_|X|X|X|_|_|X|X| 3 2         
-- |_|_|_|_|_|_|_|_| 2 2       |_|_|X|X|_|_|X|X| 2 2         
-- |_|_|_|_|_|_|_|_| 6         |_|_|X|X|X|X|X|X| 6           
-- |_|_|_|_|_|_|_|_| 1 5       |X|_|X|X|X|X|X|_| 1 5         
-- |_|_|_|_|_|_|_|_| 6         |X|X|X|X|X|X|_|_| 6           
-- |_|_|_|_|_|_|_|_| 1         |_|_|_|_|X|_|_|_| 1           
-- |_|_|_|_|_|_|_|_| 2         |_|_|_|X|X|_|_|_| 2           
--  1 3 1 7 5 3 4 3             1 3 1 7 5 3 4 3              
--  2 1 5 1                     2 1 5 1      
--
data Grid = Dot | Null
type Line = [Grid]
type Table = [Line]

genLines :: Int -> [Int] -> [Line]
genLines n [] = [replicate n Null]
genLines n [x] = do
    let ns = replicate (n - x) Null
    (l1, l2) <- zip (inits ns) (tails ns)
    let dl =  replicate x Dot
    return $ l1 ++ dl ++ l2
genLines n l@(x:xs) = do
    let size = length l
    let dots_sum = sum l
    let padding = n - dots_sum - (size - 1)
    n' <- [x..(x + padding)]
    l1 <- genLines n' [x]
    l2 <- genLines (n - n' - 1) xs
    return $ l1 ++ [Null] ++ l2

rightLine :: Line -> [Int] -> Bool
rightLine [] [] = True
rightLine (Null:gs) ns = rightLine gs ns
rightLine (Dot:gs) ns = case (gs, ns) of
    ([], [1]) -> True
    (_, 0:_)  -> False
    (Null:gs', 1:ns') -> rightLine gs' ns'
    (Dot:gs', n:ns')  -> rightLine gs' ((n - 1):ns')
    _ -> False

genTables :: [[Line]] -> [Table]
genTables [] = []
genTables (ls:lss) = do
    l <- ls
    t <- genTables lss
    return (l:t)

rightTable :: Table -> [[Int]] -> Bool
rightTable t ns = let t' = transpose t in
    all (\(l, n) -> rightLine l n) (zip t' ns)

nonogram' :: [[Int]] -> [[Int]] -> [Table]
nonogram' hs vs = filter (\t -> rightTable t vs) possible_tables
  where
    width = length hs
    possible_tables = genTables $ map (genLines width) hs

show_grid :: Grid -> String
show_grid Null = "_"
show_grid Dot = "X"

show_line :: Line -> String
show_line l = "|" ++ concat (intersperse "|" $ map show_grid l) ++ "|\n"

show_table :: Table -> String
show_table = concatMap show_line

nonogram :: [[Int]] -> [[Int]] -> [String]
nonogram hs vs = map show_table $ nonogram' hs vs

-- Problem 99
-- Crossword puzzle
-- Given an empty (or almost empty) framework of a crossword puzzle and a set
-- of words. The problem is to place the words into the framework.
type Coord = (Int, Int)
type CWord = String
data Site = Site {siteCoords :: [Coord], siteLen :: Int}
            deriving (Show, Eq)
data Crossword = Crossword {cwWords :: [CWord], cwSites :: [Site]}
                  deriving (Show, Eq)

-- merge a word and a site by assigning each letter to its respective coordinate
together :: (CWord, Site) -> [(Coord, Char)]
together (w, s) = zip (siteCoords s) w

-- test that every coordinate has only one kind of letter.
noCollision :: [(CWord, Site)] -> Bool
noCollision xs = all all_equal groupByCoord
  where
    -- groupByCoord :: [[Char]]
    groupByCoord =
        map (map snd) . groupBy eq_on_fst . sortBy compare_first . concatMap together $ xs
    eq_on_fst p1 p2 = fst p1 == fst p2
    compare_first p1 p2 = compare (fst p1) (fst p2)
    all_equal [] = True
    all_equal (x:xs') = all (x ==) xs'    

-- all the possible combinations of words and coordinates.
solver' :: [CWord] -> [Site] -> [[(CWord, Site)]]
solver' _ [] = [[]]
solver' ws (s:ss) =
    if null possWords
      then error ("too few words of length " ++ show (siteLen s))
      else do
        try <- possWords
        let restWords = delete try ws
        more <- solver' restWords ss
        let attempt = (try, s):more
        guard $ noCollision attempt
        return attempt
  where
    possWords = filter (\w -> siteLen s == length w) ws

-- return all solutions for the corssword as lists of occupied coordinates and
-- their respective letters
solver :: Crossword -> [[(Coord, Char)]]
solver cw = map (concatMap together) solutions
  where
    solutions = solver' (cwWords cw) (cwSites cw)

-- read the content of a file into the "Crossword" datatype.
readCrossword :: String -> Crossword
readCrossword s = Crossword ws ss
  where
    (ws, strs) = break ("" ==) $ lines s
    ss = toSites $ tail strs

-- convert the text lines from the file to the "Site" datatype, which contain
-- the adjacent coordinates of the site and its length.
toSites :: [String] -> [Site]
toSites ls = map mk_site . filter is_site $ ls'
  where
    ls' = v_coords ++ h_corrds
    v_coords = concatMap (groupBy eq_on_snd) v_coords'
    h_corrds = concatMap (groupBy eq_on_snd) h_corrds'
    eq_on_snd p1 p2 = snd p1 == snd p2
    v_coords' = do
        let cls = zip [1..] (map (zip [1..]) ls)
        (y, xcs) <- cls
        return $ map (\(x, c) -> ((x, y), c)) xcs
    h_corrds' = transpose v_coords'
    is_site [] = False
    is_site [x] = False
    is_site ((_, c):_) = c /= ' '
    mk_site xs = Site (map fst xs) (length xs)



