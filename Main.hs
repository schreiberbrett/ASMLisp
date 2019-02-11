module Main where

import Data.Char (isDigit)
import Data.List (intercalate, isPrefixOf, partition)
import Data.Set (Set, member, union, insert, singleton)
import Data.Maybe (fromMaybe)
import RISCV (registers, primitiveInstructions)
import Symbol (toSymbols)
import Instruction (parseInstruction, Instruction (..), Argument(..), Identifier)

data Definition =
	Predefined String |
	ImmediateDefinition Int |
	LabelDefinition String |
	Function Parameters [Instruction] Scope

type Parameters = [Identifier]
type Scope = [(Identifier, Definition)]
type Labels = Int
type LocalScope = Scope
type GlobalScope = Scope
data Context = Context LocalScope GlobalScope (Set String) Labels

addLocal :: (Identifier, Definition) -> Context -> Context
addLocal definition (Context locals globals visited labels) =
	Context (definition : locals) globals visited labels

visit :: String -> Context -> Context
visit string (Context locals globals visited labels) =
	Context locals globals (string `insert` visited) labels

wasVisited :: String -> Context -> Bool
wasVisited string (Context _ _ visited _) = string `member` visited

predefinedIdentifiers :: Set String
predefinedIdentifiers = registers `union` primitiveInstructions

generateInstructions :: [Instruction] -> Context -> (Int, String)
generateInstructions [] (Context _ _ _ labels) = (labels, "")

generateInstructions (Define id arg:rest) context@(Context locals globals _ _) =
	generateInstructions rest
		((id, resolveArg arg (locals ++ globals)) `addLocal` context)

generateInstructions (Label id:rest) (Context locals globals visited labelNum) =
	(label ++ ":\n") +++ generateInstructions rest (Context
		((id, LabelDefinition label) : locals)
		globals
		visited
		(labelNum + 1)) where
	label = 'L' : show labelNum

generateInstructions (Call id args:rest) context@(Context locals globals visited labels)
	| id `member` visited = error $ "Circular call to " ++ id
	| id `member` primitiveInstructions =
		("\t" ++ id ++ " " ++ intercalate ", " primitives ++ "\n") +++
		generateInstructions rest context
	| otherwise = functionCallCode +++ generateInstructions rest
		(Context locals globals visited newLabels) where
	(newLabels, functionCallCode) = generateInstructions lambdaInstructions
		(Context (parameterDefinitions ++ closureScope) globals visited labels)
	primitives = map (`resolvePrimitiveArg` scope) args
	(parameters, lambdaInstructions, closureScope) = resolveFunction id scope
	parameterDefinitions :: Scope
	parameterDefinitions = zip parameters (map (`resolveArg` scope) args)
	scope = locals ++ globals


a +++ b = (fst b, a ++ snd b)

resolveArg :: Argument -> Scope -> Definition
resolveArg (Immediate i) _ = ImmediateDefinition i
resolveArg (Lambda params instructions) scope = Function params instructions scope
resolveArg (Referenced id) scope = resolve id scope

resolvePrimitiveArg :: Argument -> Scope -> String
resolvePrimitiveArg (Immediate int) _ = show int
resolvePrimitiveArg (Referenced id) definitions = case resolve id definitions of
	Predefined str -> str
	ImmediateDefinition int -> show int
	LabelDefinition str -> str
	_ -> error "Passed function `" ++ id ++ "`, but expected a primitive."
resolvePrimitiveArg _ _ = error "Expected primitive argument but got lambda."

resolveFunction :: Identifier -> Scope -> (Parameters, [Instruction], Scope)
resolveFunction identifier definitions = case resolve identifier definitions of
	Function params instructions scope -> (params, instructions, scope)
	_ -> error $ "`" ++ identifier ++ "` is not a function."

resolve :: Identifier -> Scope -> Definition
resolve identifier definitions
	| identifier `member` predefinedIdentifiers = Predefined identifier
	| otherwise = fromMaybe
		(error $ "`" ++ identifier ++ "`not defined.")
		(lookup identifier definitions)

generateCode :: [Instruction] -> String
generateCode instructions = case mainDefinition of 
	[Define _ (Lambda [] mainInstructions)] ->
		snd $ generateInstructions mainInstructions $ Context [] globals (singleton "main") 0
	_ -> error "`main` defined 0 or more than 2 times, or with incorrect parameters."
	where
	(mainDefinition, rest) = partition isMain instructions 
	isMain (Define "main" _) = True
	isMain _ = False
	globals = map (\x -> case x of
			Define id arg -> (id, resolveArg arg [])
			_ -> error "Top level instructions must be definitions.") rest

main :: IO ()
main = interact (generateCode . map parseInstruction . toSymbols)
-- main = print registerNames
