module Main where

import Lib

showDemo a b c = a $ b ( c * c )

main :: IO ()
main = someFunc