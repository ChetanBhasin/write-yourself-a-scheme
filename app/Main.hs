module Main where

import SchemeEvaluator
import System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
