module Main where

import ASMLispParser (parseASMLisp)
import CodeGeneration (generateCode)

main :: IO ()
main = interact (generateCode . parseASMLisp)