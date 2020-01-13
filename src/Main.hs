module Main where

import BFInterpreter
import Parser

main :: IO ()
main = interact exec >> putStr "\n"


