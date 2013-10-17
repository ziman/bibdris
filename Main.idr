module Main

import System
import Bibtex

import Lightyear.String_

usage : IO ()
usage = putStrLn "usage: bibdris db.bib"

processFile : String -> IO ()
processFile fn = do
  stuff <- readFile fn
  case parse bibtex stuff of
    Success s es => putStrLn (show es)
    Failure es => putStrLn (show es)

main : IO ()
main = getArgs >>= processArgs
  where
    processArgs (_ :: fn :: []) = processFile fn
    processArgs _ = usage
