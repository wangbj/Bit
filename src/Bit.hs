{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Bit (
    BIT
  , fromList
  , update
  , sumUpto
  , sumRange
  ) where

import Data.Bits
import Data.Monoid
import Control.Monad

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Vector.Unboxed(Vector)

import Control.Monad.ST

parent x = x .&. (x-1)
next x = x + (x .&. (-x))

newtype BIT = BIT (Vector Int, Vector Int)

instance Show BIT where
  show (BIT (v, w)) = show v

instance Eq BIT where
  BIT (u, w) == BIT (u', w') = u == u'

fastUpdate u w hi j = do
  let i = pred j
      xs = takeWhile (<= hi) . iterate next $ j
  mapM_ acc xs
    where acc k = MU.unsafeRead w k >>= \v ->
            MU.unsafeWrite w k ( (u U.! (pred j)) + v )

fromListU :: Vector Int -> Vector Int
fromListU u = runST $ do
  let len = U.length u 
      w = U.fromList (replicate (1+len) 0)
  w' <- U.unsafeThaw w
  mapM_ (fastUpdate u w' len) [1..len]
  w'' <- U.unsafeFreeze w'
  return w''

fromList xs = BIT (u, w)
  where u = U.fromList xs
        w = fromListU u

updateFenwick w hi i x = do
  when (i <= hi) (
    MU.unsafeRead w i >>= \v -> 
    MU.unsafeWrite w i (v+x) >>
    updateFenwick w hi (next i) x
    )

update (BIT (u, w)) j v = runST $ do
  let hi = U.length u
      i = pred j
  u' <- U.unsafeThaw u
  w' <- U.unsafeThaw w
  let !x = v - u U.! i
  MU.unsafeWrite u' i v
  u'' <- U.unsafeFreeze u'
  updateFenwick w' hi j x
  w'' <- U.unsafeFreeze w'
  return $! BIT (u'', w'')

sumUpto :: BIT -> Int -> Int
sumUpto bit@(BIT (u, w)) k
  | k == 0 = 0
  | otherwise = (w U.! k) + sumUpto bit (parent k)

sumRange :: BIT -> Int -> Int -> Int
sumRange bit@(BIT (u, w)) i j
  | i > j = sumRange bit j i
  | i == 0 = sumUpto bit j
  | otherwise = sumUpto bit j - sumUpto bit (i-1)
