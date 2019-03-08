-- | The definition and functionality of the 'Instruction' data type.
module Instruction
    ( Instruction (..)
    , Argument (..)
    , Identifier
    , Parameters
    ) where

-- | The fundamental unit of execution in ASMLisp. An instruction corresponds to
-- an assembly instructions with the ability to specify complex behavior or
-- arguments.
data Instruction
    = Define Identifier Argument  -- ^ Add a local definition.
    | Label Identifier            -- ^ Add a local label.
    | Call Identifier [Argument]  -- ^ Call an instruction on specified arguments.
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
