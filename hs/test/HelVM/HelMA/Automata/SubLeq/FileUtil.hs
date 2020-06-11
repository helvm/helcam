module HelVM.HelMA.Automata.SubLeq.FileUtil (
  readSqFile
) where

readSqFile :: String -> IO String
readSqFile fileName = readFile $ buildAbsoluteSqFileName fileName

buildAbsoluteSqFileName :: String -> String
buildAbsoluteSqFileName fileName = sqDir <> fileName <> ".sq"

sqDir :: String
sqDir = dir <> "sq/"

dir :: String
dir = "examples/"
