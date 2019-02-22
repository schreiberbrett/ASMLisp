-- | ASMLisp primitives specific to the RISC-V assembly architecture.
module RISCV
    ( registers
    , primitiveInstructions
    ) where

import Data.Set (Set, fromList)

-- | RISC-V has 32 general purpose registers, including a dedicated zero @x0@, a
-- stack pointer @sp@, and more.
registers :: Set String
registers = fromList $
    allOf 'x' [0..31] ++
    allOf 's' [0..11] ++
    allOf 'a' [0..7] ++
    allOf 't' [0..6] ++
    ["ra", "sp", "gp", "tp", "fp", "zero"]
      where
        allOf char range = [char : show i | i <- range]

-- | RISC-V has a number of primitive instructions as well, including @add@,
-- @sub@, and @beq@.
primitiveInstructions :: Set String
primitiveInstructions = fromList
    [ "lui"
    , "auipc"
    , "jal", "jalr"
    , "beq", "bne", "blt", "bge", "bltu", "bgeu"
    , "lb", "lh", "lw", "lbu", "lhu"
    , "sb", "sh", "sw"
    , "addi"
    , "slti", "sltiu"
    , "xori", "ori", "andi"
    , "slli", "srli", "srai"
    , "add", "sub", "sll"
    , "slt", "sltu"
    , "xor", "or", "and"
    , "srl", "sra"
    , "fence", "fence.i"
    , "ecall", "ebreak"
    , "csrrw", "csrrs", "csrrc"
    , "csrrwi", "csrrsi", "csrrci"
    ]
