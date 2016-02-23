import Control.Applicative
import Test.QuickCheck

import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed(Vector)
import Criterion.Main
import Data.Monoid
import System.Random
import Control.Monad

import Bit

data Cmd = U Int Int |
           Q Int Int deriving Show

instance Arbitrary Cmd where
  arbitrary = elements [0,1] >>= \c ->
    choose (1, 1000) >>= \idx ->
    choose (1, 1000) >>= \v ->
    if c == 0 then return $! U idx v else return $! Q (min idx v) (max idx v)

data Test = Test [Int] [Cmd]
            deriving Show

instance Arbitrary Test where
  arbitrary = liftA2 Test arr cmd
    where arr = fmap (take 1000) infiniteList
          cmd = arbitrary 

sumFromTo :: Vector Int -> Int -> Int -> Int
sumFromTo u i j = getSum (go i)
  where go k
          | k > j = mempty
          | otherwise = Sum (fromIntegral (u U.! k)) <> go (succ k)

bruteforce xs = reverse . snd . foldl go (u, []) 
  where u = U.fromList (0:xs)
        go (u, r) (Q i j) = (u, sumFromTo u i j : r)
        go (u, r) (U k v) = (u U.// [(k, v)], r)

fenwick xs = reverse . snd . foldl go (bit, [])
  where bit = fromList xs
        go (bit, r) (Q i j) = (bit, sumRange bit i j : r)
        go (bit, r) (U k v) = (update bit k v, r)

prop_result_equal_bruteforce :: Test -> Bool
prop_result_equal_bruteforce (Test xs qs) =
  fenwick xs qs == bruteforce xs qs

randomList :: Int -> IO [Int]
randomList k = do
  g <- getStdGen
  return . take k. randomRs (1, 10^6) $ g

randomQuery :: IO Cmd
randomQuery = do
  g <- getStdGen
  let (i, g') = randomR (1, 1000) g
      (j, g'') = randomR (1, 1000) g'
  c <- randomIO :: IO Int
  if c >= 0 then return (Q (min i j) (max i j)) else
    return (U i j)

randomQueries :: Int -> IO [Cmd]
randomQueries k = replicateM k (randomQuery)

main :: IO ()
main = do
  xs <- randomList 1000
  qs <- randomQueries 10000
  defaultMain [
    bgroup "array update & query test" [
          bench "naive method" $ whnf (bruteforce xs) qs
        , bench "with fenwick" $ whnf (fenwick xs) qs
        ]
    ]
