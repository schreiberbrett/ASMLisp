module Instruction (parseInstruction, Instruction (Define, Label, Call), Argument (Referenced, Lambda, Immediate), Identifier) where

import Data.Char (isDigit)
import Symbol (Symbol (Atom, Symbols), isAtom)

data Instruction = Define Identifier Argument | Label Identifier | Call Identifier [Argument]
data Argument = Referenced Identifier | Lambda [Identifier] [Instruction] | Immediate Int
type Identifier = String

instance Show Instruction where
	show instruction = "(" ++ toString instruction ++ ")" where
		toString (Define id arg) = "define " ++ id ++ " " ++ show arg
		toString (Label id) = "label " ++ id
		toString (Call id []) = id
		toString (Call id args) = id ++ " " ++ (unwords . map show) args

instance Show Argument where
	show (Referenced id) = id
	show (Lambda params instructions) =
		"(lambda " ++ unwords params ++ "\n\t" ++
		concatMap show instructions ++ ")"
	show (Immediate int) = show int

parseInstruction :: Symbol -> Instruction
parseInstruction (Symbols [Atom "define", id, arg]) =
	Define (parseIdentifier id) (parseArgument arg)
parseInstruction (Symbols [Atom "label", id]) = Label (parseIdentifier id)
parseInstruction (Symbols (id:args)) = 	Call (parseIdentifier id) (map parseArgument args)

parseArgument :: Symbol -> Argument
parseArgument (Atom a) =
	if ((\c -> isDigit c || c == '-') . head) a
	then Immediate $ read a
	else Referenced a

parseArgument (Symbols (Atom "lambda" : idsAndInstructions)) = 
	Lambda (map parseIdentifier ids) (map parseInstruction instructions) where
	(ids, instructions) = span isAtom idsAndInstructions

parseArgument (Symbols (Atom "λ" : idsAndInstructions)) =
	Lambda (map parseIdentifier ids) (map parseInstruction instructions) where
	(ids, instructions) = span isAtom idsAndInstructions

parseIdentifier :: Symbol -> Identifier
parseIdentifier (Atom a) = a
parseIdentifier symbols = error $ "Expected identifier but got: " ++ show symbols

