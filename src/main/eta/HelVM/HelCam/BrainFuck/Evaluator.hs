{-# Language FlexibleInstances #-}
module HelVM.HelCam.BrainFuck.Evaluator where

import HelVM.HelCam.BrainFuck.Symbol
import HelVM.HelCam.BrainFuck.TableOfInstructions
import HelVM.HelCam.BrainFuck.TapeOfSymbols
import HelVM.HelCam.BrainFuck.Token
import HelVM.HelCam.BrainFuck.Lexer

import HelVM.HelCam.Common.Util

import Data.Int
import Data.Word

class Evaluator r where
  evalBFInt8 :: Source -> r
  evalBFInt8 = flip evalBF (newTape :: FullTape Int8)

  evalBFWord8 :: Source -> r
  evalBFWord8  = flip evalBF (newTape :: FullTape Word8)

  evalBF :: Symbol s => Source -> FullTape s -> r
  evalBF source = doInstruction ([], tokenizeBF source)

  doInstruction :: Symbol s => Table -> FullTape s -> r
  doInstruction table@(_, MoveR   :_) tape = doInstruction    (nextInst table) (moveHeadRight tape)
  doInstruction table@(_, MoveL   :_) tape = doInstruction    (nextInst table)  (moveHeadLeft tape)
  doInstruction table@(_, Inc     :_) tape = doInstruction    (nextInst table)   (wSuccSymbol tape)
  doInstruction table@(_, Dec     :_) tape = doInstruction    (nextInst table)   (wPredSymbol tape)
  doInstruction table@(_, JmpPast :_) tape = doJmpPast                  table                 tape
  doInstruction table@(_, JmpBack :_) tape = doJmpBack                  table                 tape
  doInstruction table@(_, Output  :_) tape = doOutput                   table                 tape
  doInstruction table@(_, Input   :_) tape = doInput                    table                 tape
  doInstruction       (_, []        ) _    = doEnd

  doJmpPast :: Symbol s => Table -> FullTape s -> r
  doJmpPast table tape@(_, 0:_) = doInstruction (jumpPast table) tape
  doJmpPast table tape          = doInstruction (nextInst table) tape

  doJmpBack :: Symbol s => Table -> FullTape s -> r
  doJmpBack table tape@(_, 0:_) = doInstruction (nextInst table) tape
  doJmpBack table tape          = doInstruction (jumpBack table) tape

  doOutput  :: Symbol s => Table -> FullTape s -> r
  doInput   :: Symbol s => Table -> FullTape s -> r
  doEnd     :: r

----

interactEvalBF :: Source -> IO ()
interactEvalBF source = interact (evalBFWord8 source)

batchEvalBFInt8 :: Source -> Output
batchEvalBFInt8 = flip evalBFInt8 ([]::String)

batchEvalBFWord8 :: Source -> Output
batchEvalBFWord8  = flip evalBFWord8 ([]::String)

instance Evaluator Interact where
  doInput _     _          []     = error "Empty input"
  doInput table tape (char:input) = doInstruction (nextInst table) (writeSymbol char tape) input

  doOutput _          (_, [])       _     = error "Illegal State"
  doOutput table tape@(_, symbol:_) input = toChar symbol : doInstruction (nextInst table) tape input

  doEnd _ = []

----

monadicEvalBF :: Source -> IO ()
monadicEvalBF = evalBFWord8

instance Evaluator (IO ()) where
  doInput table tape = do
    char <- getChar
    doInstruction (nextInst table) (writeSymbol char tape)

  doOutput _          (_, [])       = error "Illegal State"
  doOutput table tape@(_, symbol:_) = do
    putChar $ toChar symbol
    doInstruction (nextInst table) tape

  doEnd = return ()