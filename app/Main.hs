module Main where

import Test.QuickCheck

import Bit

ex1 = [1..10]

main :: IO ()
main = print . fromList $ ex1
