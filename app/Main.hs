module Main where

import Lib

main :: IO ()
main = autoPlay (initialMcts emptyBoard SQBlack SQEmpty) 1000
