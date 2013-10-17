module Main

import System
import Bibtex

import Lightyear.String_

processEntries : List Entry -> IO (List Entry)
processEntries es = do
  putStrLn $ format es
  return es

usage : IO ()
usage = putStrLn "usage: bibdris db.bib"

writeFile : String -> String -> IO ()
writeFile fn stuff = do
  f <- openFile fn Write
  fwrite f stuff
  closeFile f

processFile : String -> IO ()
processFile fn = do
  stuff <- readFile fn
  case parse bibtex stuff of
    Success s es => do
      es' <- processEntries es
      writeFile fn $ format es'
    Failure es => putStrLn (show es)

main : IO ()
main = getArgs >>= processArgs
  where
    processArgs (_ :: fn :: []) = processFile fn
    processArgs _ = usage
