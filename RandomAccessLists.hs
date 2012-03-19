module CS457.Project.RandomAccessList (
  RandomAccessList(..),
  BinaryRandomAccessList,
  SkewRandomAccessList
  ) where

import Prelude hiding (head,tail,lookup)


class RandomAccessList r where
  empty   :: r a
  isEmpty :: r a -> Bool
  cons    :: r a -> r a
  head    :: r a -> a
  tail    :: r a -> r a
  lookup  :: r a -> Int -> a
  update  :: r a -> Int -> a -> r a 


data Tree a = Leaf a | Node Int (Tree a) (Tree a)
data Digit a = Zero | One (Tree a) 
newtype BinaryRandomAccessList a = BArray [Digit a]

-- BinaryRandomAccessList helper functions

size (Leaf x)     = 1
size (Node w x y) = w
link x y = Node (size x + size y) x y

consTree x []           = [One x]
consTree x (Zero : xs)  = One x : xs
consTree x (One y : ys) = Zero : consTree (link x y) xs

unconsTree [] = error "BinaryRandomAccessList: Can't uncons empty list"
unconsTree [One x] = (x,[])
unconsTree (One x : xs) = (x, Zero:xs)
unconsTree (Zero:xs) = (x, One y : xs')
    where (Node _ x y, xs') = unconsTree xs

instance RandomAccessList BinaryRandomAccessList where
  empty                = BArray []
  isEmpty (BArray xs)  = null xs
  cons x (BArray xs)   = BArray $ consTree (Leaf x) xs
  head (BArray xs)     = let (Leaf X, _) = unconsTree xs in x
  tail (BArray xs)     = let (_,xs') = unconsTree xs in BArray xs'
  lookup i (BArray Zero:xs) = lookup i xs
  lookup i (BArray (One t):xs)  = if i<size t then lookupTree t i else lookup i (BArray xs)
        where lookupTree (Leaf x) i = x
              lookupTree (Tree w t u) = if i<(w `div` 2) then lookupTree t i else lookupTree u (i-w `div` 2)



data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show
newtype SkewRandomAccessList a = SK[(Int, Tree a)]

instance RandomAccessList SkewRandomAccessList where
  cons x (SK ((i,xs):(j,ys):ts))
      | i==j     = SK((1+i+j, Node x xs ys):ts)
  cons x (SK ts) = SK((1, Leaf X):ts)