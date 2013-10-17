module Main

import System
import Bibtex

import Lightyear.String_

Url : Type
Url = String

data Args = List | Add Url

addUrl : Url -> IO Entry
addUrl = addUrl

listEntries : List Entry -> IO ()
listEntries = traverse_ $ putStrLn . fmt
  where
    field : String -> List Item -> String
    field n = fromMaybe "?" . map value . Prelude.List.find (\(It k v) => k == n)

    fmt : Entry -> String
    fmt (En ty id its) = id ++ "\t" ++ field "author" its ++ "\t" ++ field "title" its

(>>) : Monad m => m a -> m b -> m b
x >> y = x >>= \_ => y

processEntries : Args -> List Entry -> IO (List Entry)
processEntries  List     es = listEntries es >> return es
processEntries (Add url) es = (:: es) <@> addUrl url

usage : IO ()
usage = putStrLn "usage: bibdris db.bib (-a <url> | -l)"

writeFile : String -> String -> IO ()
writeFile fn stuff = do
  f <- openFile fn Write
  fwrite f stuff
  closeFile f

processFile : Args -> String -> IO ()
processFile args fn = do
  stuff <- readFile fn
  case parse bibtex stuff of
    Success s es => do
      es' <- processEntries args es
      writeFile fn $ format es'
    Failure es => putStrLn (show es)

main : IO ()
main = getArgs >>= processArgs
  where
    processArgs (_ :: fn :: "-a" :: url :: [])
      = processFile (Add url) fn

    processArgs (_ :: fn :: "-l" :: [])
      = processFile List fn

    processArgs _
      = usage
