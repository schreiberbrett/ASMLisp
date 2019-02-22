-- | The definition and functionality of the 'Instruction' data type.
module Instruction
    ( parseInstruction
    , Instruction (..)
    , Argument (..)
    , Identifier
    , Parameters
    ) where

import Data.Char (isDigit)
import Symbol (Symbol (Atom, Symbols), isAtom)

-- | The fundamental unit of execution in ASMLisp. An instruction corresponds to
-- an assembly instructions with the ability to specify complex behavior or
-- arguments.
data Instruction
    = Define Identifier Argument  -- ^ Add a local definition.
    | Label Identifier            -- ^ Add a local label.
    | Call Identifier [Argument]  -- ^ Call a user-defined instruction on specified arguments.
    | Primitive Identifier [Argument] -- ^ Call an assembly-defined instruction on specified arguments.
    deriving Show
-- | The right-hand-side of any instruction in ASMLisp.
data Argument
    -- | Either a primitive register, or an identifier defined earlier in the
    -- source.
    = Referenced Identifier
    -- | An anonymous parameterized list of instructions.
    | Lambda Parameters [Instruction]  
    -- | An integer corresponding to immediate values used in assembly.
    | Immediate Int
    deriving Show
-- | A symbolic name used to reference a previously-defined 'Identifier', a
-- complex 'Instruction' (as a 'Lambda'), a 'Label', a register, or an Immediate
type Identifier = String

-- | A list of 'Identifier's specifically used to represent the parameters of a
-- 'Lambda' 'Argument'
type Parameters = [Identifier]

{-
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
-}

-- | Convert a 'Symbol' into the 'Instruction' that it represents.
parseInstruction
    :: Symbol       -- ^ The 'Symbol' representing an 'Instruction'
    -> Instruction  -- ^ The representative 'Instruction'
parseInstruction (Symbols [Atom "define", id, arg]) =
    Define (parseIdentifier id) (parseArgument arg)
parseInstruction (Symbols [Atom "label", id]) = Label (parseIdentifier id)
parseInstruction (Symbols (id:args)) = Call (parseIdentifier id) (map parseArgument args)
parseInstruction symbol = error $ show symbol

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