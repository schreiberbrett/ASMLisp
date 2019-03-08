-- | A singleton module exporting 'generateCode'
module CodeGeneration (generateCode) where


import Data.List (intercalate, isPrefixOf, partition)
import Data.Set (Set, member, union, insert, singleton)
import Data.Maybe (fromMaybe)
import RISCV (registers, primitiveInstructions)
import Instruction (Instruction (..), Argument(..), Identifier, Parameters)

data Definition
    = Predefined String
    | ImmediateDefinition Int
    | LabelDefinition String
    | Function Parameters [Instruction] Scope

type Scope = [(Identifier, Definition)]
type Labels = Int
type LocalScope = Scope
type GlobalScope = Scope
data Context = Context LocalScope GlobalScope (Set String) Labels

{-
type Code = String
type Scope' = Map Identifier Definition'
data Context' = Context' Code LocalScope' GlobalScope' (Set String) Labels


data Definition' = Primitive String | Function Parameters [Instruction] Scope
data ResolvedArgument = Predefined Definition | ForwardReferencedLabel String

generateInstructions' :: [Instruction] -> Context' -> Context'
generateInstructions' [] context = context

generateInstructions' (Define identifier argument):rest (Context' code locals globals visited labelNum) =
    generateInstructions rest $ case resolve argument of
        Predefined definition -> Context' code (insert identifier definition locals) globals visited labelNum
        ForwardReferencedLabel str -> Context' code (Map.insert identifier (LabelDefinition str) locals) globals visited (labelNum + 1)

generateInstructions' (Label identifier):rest (Context' code locals globals visited labelNum) =
    generateInstructions rest $ case resolve (Referenced identifier) of
        Predefined (Primitive label) -> Context' (code ++ label ++ ":\n") locals globals visited labelNum
        Predefined (Function _) -> error $ "Expected a label, but got function `" ++ identifier ++ "`"
        ForwardReferencedLabel str -> Context' (code ++ str ++ ":\n") (Map.insert identifier (labelDefinition str) locals) globals visited (labelNum + 1)

generateInstructions' (Call identifier arguments):rest (Context' code locals globals visited labelNum) =
    | identifier `member` visited = error $ "Circular call to " ++ identifier
    | identifier `member` primitiveInstructions = generateInstructions' rest 
        Context'
            (code ++"\t" ++ identifier ++ " " ++ intercalate ", " map toString resolvedArguments)
            resolvedLocals globals visited resolvedLabelNum
    | otherwise = generateInstructions' rest newContext
      where
        (Context functionCode _ _ _ newLabelNum2) = generateInstructions function parameterArguments
-}
-- | Push a definition to the local scope of a context
addLocal
    :: (Identifier, Definition)  -- ^ Identifier and corresponding definition
    -> Context                   -- ^ Current context
    -> Context                   -- ^ New context
addLocal definition (Context locals globals visited labels) =
    Context (definition : locals) globals visited labels

predefinedIdentifiers :: Set String
predefinedIdentifiers = registers `union` primitiveInstructions

generateInstructions :: [Instruction] -> Context -> (Int, String)
generateInstructions [] (Context _ _ _ labels) = (labels, "")

generateInstructions (Define id arg:rest) context@(Context locals globals _ _) =
    generateInstructions rest
        ((id, resolveArg arg (locals ++ globals)) `addLocal` context)

generateInstructions (Label id:rest) context =
    (label ++ ":\n") +++ generateInstructions rest newContext
      where
        (label, newContext) = resolvePrimitiveArg (Referenced id) context

generateInstructions (Call id args:rest) context@(Context locals globals visited labels)
    | id `member` visited = error $ "Circular call to " ++ id
    | id `member` primitiveInstructions =
        ("\t" ++ id ++ " " ++ intercalate ", " primitives ++ "\n") +++
        generateInstructions rest newContext
    | otherwise = functionCallCode +++ generateInstructions rest
        (Context locals globals visited newLabels)
      where
        (newLabels, functionCallCode) = generateInstructions lambdaInstructions
            (Context (parameterDefinitions ++ closureScope) globals visited labels)
        (primitives, newContext) = resolvePrimitiveArgs args context
        resolvePrimitiveArgs [] firstContext = ([], firstContext)
        resolvePrimitiveArgs (first:rest) firstContext = (firstResolved:restResolved, restContext)
          where
            (firstResolved, newContext) = resolvePrimitiveArg first firstContext
            (restResolved, restContext) = resolvePrimitiveArgs rest newContext

        (parameters, lambdaInstructions, closureScope) = resolveFunction id scope
        parameterDefinitions :: Scope
        parameterDefinitions = zip parameters (map (`resolveArg` scope) args)
        scope = locals ++ globals



-- Given an argument and a context, return the primitive register or label corresponding to that argument.
-- If one doesn't exist, then create a new one.
resolvePrimitiveArg :: Argument -> Context -> (String, Context)

resolvePrimitiveArg (Immediate int) context = (show int, context)

resolvePrimitiveArg (Referenced identifier) context@(Context locals globals visited labelNum)
    | identifier `member` predefinedIdentifiers = (identifier, context)
    | otherwise = case lookup identifier $ locals ++ globals of

        Just definition -> (resultOf definition, context)
        -- If primitive argument not found, assume it is a label defined later.
        Nothing -> (newLabel, newContext)

      where
        resultOf def = case def of
            Predefined str -> str
            ImmediateDefinition int -> show int
            LabelDefinition str -> str
            Function _ _ _ -> error "Passed function `" ++ identifier ++ "`, but expected a register, immediate, or label."

        newLabel = 'L' : show labelNum
        newContext = Context newLocals globals visited newLabelNum
        newLocals = (identifier, LabelDefinition newLabel):locals
        newLabelNum = labelNum + 1

resolvePrimitiveArg _ _ = error "Expected a register, immediate, or label, but got lambda."

a +++ b = (fst b, a ++ snd b)

resolveArg :: Argument -> Scope -> Definition
resolveArg (Immediate i) _ = ImmediateDefinition i
resolveArg (Lambda params instructions) scope = Function params instructions scope
resolveArg (Referenced id) scope = resolve id scope

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


-- | Generate a list of ASMLisp instructions into a string of assembly instructions
generateCode
    :: [Instruction]  -- ^ The ASMLisp instructions
    -> String         -- ^ The generated assembly
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
