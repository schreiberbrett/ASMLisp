module Main where

import Symbol (toSymbols)
import Instruction (parseInstruction)
import CodeGeneration (generateCode)

main :: IO ()
main = interact (generateCode . map parseInstruction . toSymbols)