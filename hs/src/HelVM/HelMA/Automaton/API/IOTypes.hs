module HelVM.HelMA.Automaton.API.IOTypes where

type Source = Text
type Input  = Text
type Output = Text

type Interact = Input -> Output

emptyInput :: Input
emptyInput = ""
