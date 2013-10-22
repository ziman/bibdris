module Main

import System
import Bibtex
import Utils

import Lightyear.String_

Url : Type
Url = String

data Args = List | Add Url

data LazyList : Type -> Type where
  Ni : LazyList a
  Co : (x : a) -> |(xs : LazyList a) -> LazyList a

inputBlock : IO (List String)
inputBlock = do
  ln <- trim <@> getLine
  case ln of
    "." => return []
    _   => (ln ::) <@> inputBlock

manualEntry : IO Entry
manualEntry = do
  putStrLn "Switching to manual entry:"
  manualEntry

generateId : String -> String -> List String -> String
generateId author year taken = ?generateId

download : String -> Url -> IO ()
download idt url = ?download

inputEntry : IO Entry
inputEntry = do
  putStrLn "Put BibTeX here, end with a '.' on an empty line."
  stuff <- cat <@> inputBlock
  case stuff of
    "" => manualEntry
    _  => case parse entry stuff of
      Success "" e => return e
      Failure es   => do
        putStrLn "BibTeX entry not recognized."
        manualEntry

addUrl : Url -> List String -> IO Entry
addUrl url takenIds = do
    En ty _ its' <- inputEntry

    let idt = generateId (find "author" "anon" its') (find "year" "" its') takenIds
    let its = update "url" url its'

    download idt url

    return $ En ty idt its

listEntries : List Entry -> IO ()
listEntries = traverse_ $ putStrLn . fmt
  where
    field : String -> List Item -> String
    field n = fromMaybe "?" . map value . Prelude.List.find (\(It k v) => k == n)

    fmt : Entry -> String
    fmt (En ty id its) = id ++ " :: " ++ field "author" its ++ " :: " ++ field "title" its

processEntries : Args -> List Entry -> IO (List Entry)
processEntries  List     es = listEntries es >> return es
processEntries (Add url) es = (:: es) <@> addUrl url (map ident es)

usage : IO ()
usage = putStrLn "usage: bibdris db.bib (-a <url> | -l)"

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
