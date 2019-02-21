module RISCV
    ( registers
    , primitiveInstructions
    ) where

import Data.Set (Set, fromList)

registers :: Set String
registers = fromList $
    allOf 'x' [0..31] ++
    allOf 's' [0..11] ++
    allOf 'a' [0..7] ++
    allOf 't' [0..6] ++
    ["ra", "sp", "gp", "tp", "fp", "zero"]
      where
        allOf char range = [char : show i | i <- range]


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
