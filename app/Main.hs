module Main where

import ASMLispParser (mainParser)
import CodeGeneration (generateCode)

main :: IO ()
main = mainParser