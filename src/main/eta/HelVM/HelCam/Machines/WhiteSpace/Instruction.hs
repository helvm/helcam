module HelVM.HelCam.Machines.WhiteSpace.Instruction where

import HelVM.HelCam.Machines.WhiteSpace.OperandParsers

import HelVM.HelCam.Common.Memories.Stack

data Instruction =
    Liter Symbol
  | Copy  Index
  | Slide Index
  | Dup
  | Swap
  | Discard
  | Binary BinaryOperator
  | Store
  | Load  --Restore
  | Mark Label
  | Call Label
  | Jump Label
  | Branch BranchTest Label
  | Return
  | OutputChar
  | OutputNum
  | InputChar
  | InputNum
  | End
  deriving (Eq, Show, Read)

type InstructionList = [Instruction]

data BinaryOperator = Add | Sub | Mul | Div | Mod
   deriving (Eq, Show, Read)

data BranchTest = EZ | Neg
   deriving (Eq, Show, Read)

----

type InstructionAddress = Int

parseIndex :: OperandParser Index
parseIndex = parseInt

type Symbol = Integer
parseSymbol :: OperandParser Symbol
parseSymbol = parseInteger

type SymbolList = [Symbol]

type Label = String
parseLabel :: Bool -> OperandParser Label
parseLabel False = parseBitString
parseLabel True  = parseAsciiString
