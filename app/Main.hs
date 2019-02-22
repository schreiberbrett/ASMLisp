module Main where

import ASMLispParser (parseProgram)
import CodeGeneration (generateCode)

main :: IO ()
main = interact (generateCode . parseProgram)