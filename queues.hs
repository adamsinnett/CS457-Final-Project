module CS457.Project.Queue (
  Queue(..),
  BankersQueue,
  RealTimeQueue,
  BatchedQueue
  ) where

import Prelude hiding (head, tail)
import QuickCheck

-- Typeclass for queue
class Queue q where
  queue   :: q a -> q a
  empty   :: q a
  isEmpty :: q a -> Bool
  snoc    :: q a -> a -> q a
  head    :: q a -> a
  tail    :: q a -> q a

-- Batched Queue
data BatchedQueue a = BatchedQueue [a] [a] deriving Show

instance Queue BatchedQueue where
  queue (BatchedQueue [] r)  =  BatchedQueue (reverse r) [] 
  queue q = q
  empty = BatchedQueue [] []
  isEmpty (BatchedQueue f _) = null f
  snoc (BatchedQueue f r) x = queue (BatchedQueue f (x:r))
  head (BatchedQueue [] _) = error "BatchedQueue: Called head on empty queue"
  head (BatchedQueue (f:fs) r) = f
  tail (BatchedQueue [] _) = error "BatchedQueue: Called head on empty queue"
  tail (BatchedQueue (f:fs) r) = queue (BatchedQueue fs r)

-- Bankers Queue
data BankersQueue a = BankersQueue [a] Int [a] Int deriving Show

instance Queue BankersQueue where
  queue q@(BankersQueue f lenF r lenR) = case (lenF<=lenR) of 
                                            True -> q
                                            False -> BankersQueue (f++(reverse r)) (lenF+lenR) [] 0 
  empty = BankersQueue [] 0 [] 0
  isEmpty (BankersQueue f _ _ _) = null f
  snoc (BankersQueue f lenF r lenR) x = queue (BankersQueue f lenF (x:r) (lenR+1) )
  head (BankersQueue [] _ _ _) = error "BankersQueue: Called head on empty queue"
  head (BankersQueue (f:fs) lenF r lenR) = f
  tail (BankersQueue [] _ _ _) = error "BankersQueue: Called tail on empty queue"
  tail (BankersQueue (f:fs) lenF r lenR) = queue (BankersQueue fs (lenF-1) r lenR)


-- Real Time Queue
data RealTimeQueue a = RealTimeQueue [a] [a] [a] deriving Show

rotate f r s = case (f,r) of
                ([],r:rs) -> (r:s)
                ((f:fs),(r:rs)) -> (f:(rotate fs rs (r:s)))

instance Queue RealTimeQueue where
  queue (RealTimeQueue f r []) = RealTimeQueue fs [] fs
                                 where fs = rotate f r []    
  queue (RealTimeQueue f r (s:ss)) = RealTimeQueue f r ss
  empty = RealTimeQueue [] [] []
  isEmpty (RealTimeQueue f _ _) = null f
  snoc (RealTimeQueue f r s) x = queue (RealTimeQueue f (x:r) s)
  head (RealTimeQueue [] _ _) = error "RealTimeQueue: Called head on empty queue"
  head (RealTimeQueue (f:fs) r s) = f
  tail (RealTimeQueue [] _ _) = error "RealtTimeQueue: Called tail on empty queue"
  tail (RealTimeQueue (f:fs) r s) = queue (RealTimeQueue fs r s)