module Main

import System

import Parser

usage : IO ()
usage = putStrLn "usage: bibdris db.bib"

processFile : String -> IO ()
processFile fn = putStrLn ("processing " ++ fn)

main : IO ()
main = getArgs >>= processArgs
  where
    processArgs (_ :: fn :: []) = processFile fn
    processArgs _ = usage
