module HelVM.HelMA.Automata.ETA.EvaluatorUtil where

import HelVM.HelMA.Automata.ETA.Token

import HelVM.HelMA.Common.OrError
import HelVM.HelMA.Common.Util

import Data.List

type Symbol = Int
type SymbolList = [Symbol]
type InstructionAddress = Int

type InstructionCounter = InstructionAddress

data InstructionUnit = IU TokenList InstructionCounter
  deriving (Show)

type OperandIUParser a = InstructionUnit -> (a , InstructionUnit)

parseNumber :: (Integral a) => OperandIUParser a
parseNumber iu = parseNumber' [] (nextIU iu)

parseNumber' :: (Integral a) => TokenList -> (Maybe Token , InstructionUnit) -> (a , InstructionUnit)
parseNumber' acc (Just E , iu) = (makeIntegral acc , iu)
parseNumber' acc (Just R , iu) = parseNumber'    acc  (nextIU iu)
parseNumber' acc (Just t , iu) = parseNumber' (t:acc) (nextIU iu)
parseNumber' acc (Nothing , iu) = (makeIntegral acc , iu)

nextIU :: OperandIUParser (Maybe Token)
nextIU iu@(IU il ic)
  | ic < length il = (Just (indexOrError ("nextIU"::Text,iu) il ic), IU il (ic+1))
  | otherwise      = (Nothing , iu)

makeIntegral :: (Integral a) => TokenList -> a
makeIntegral = foldr (mul7AndAdd . toDigit) 0

genericFindAddress :: Integral cell => TokenList -> cell -> InstructionAddress
genericFindAddress il address = findAddress il $ fromIntegral address

genericNextLabel :: Integral cell => TokenList -> InstructionAddress -> cell
genericNextLabel il ic = fromIntegral $ nextLabel il ic

----

findAddress :: TokenList -> Int -> InstructionAddress
findAddress _  1 = 0
findAddress il address = indexOrError ("findAddress"::Text,il,address) (elemIndices R (il <> [R])) (address-2) + 1

nextLabel :: TokenList -> InstructionAddress -> Int
nextLabel il ic = length (elemIndices R il') + 2  where (il',_) = splitAt ic il
