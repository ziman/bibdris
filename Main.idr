module Main

import System
import Control.Catchable

import Bibtex
import Utils

import Lightyear.String_

Url : Type
Url = String

data Args = List | Add Url

data InfList : Type -> Type where
  Co : (x : a) -> |(xs : InfList a) -> InfList a

phdRoot : String
phdRoot = "/home/ziman/phd"

phdBibtex : String
phdBibtex = phdRoot ++ "/db.bib"

phdPapers : String
phdPapers = phdRoot ++ "/papers"

inputBlock : IOE (List String)
inputBlock = do
  ln <- trim <@> ioe_lift getLine
  case ln of
    "." => return []
    _   => (ln ::) <@> inputBlock

askFor : String -> IOE (List Item -> List Item)
askFor k = do
  prn $ k ++ ":"
  v <- trim <@> ioe_lift getLine
  return $ case v of
    "" => id
    _  => (It k v ::)

manualEntry : IOE Entry
manualEntry = do
    prn "Switching to manual entry:"
    ty <- do
      prn "type [article]:"
      ty <- trim <@> ioe_lift getLine
      return $ case ty of
        "" => "article"
        _  => ty

    its <- foldr ($) Prelude.List.Nil <@> traverse askFor requiredItems
    return $ En ty "no-id-yet" its
  where
    requiredItems : List String
    requiredItems = ["title", "author", "year"]

abbreviated : String -> String
abbreviated txt = pack . map toLower $ case surnames of
    []      => unpack "unknown"
    [s] => unpack s
    _       => map firstLetter surnames
  where
    authors : List String
    authors = split (== ',') txt

    names : List (List String)
    names = map words authors

    surname : List String -> String
    surname []  = "unknown"
    surname [x] = x
    surname (x :: xs) = surname xs

    surnames : List String
    surnames = map surname names

    firstLetter : String -> Char
    firstLetter n with (strM n)
      firstLetter ""             | StrNil       = '_'
      firstLetter (strCons x xs) | StrCons x xs = x

first : (a -> Bool) -> InfList a -> a
first p (Co x xs) with (p x)
  | True  = x
  | False = first p xs

generateId : String -> String -> List String -> String
generateId author year taken = first (\x => not $ elem x taken) idents
  where
    yr : String
    yr = pack . reverse . take 2 . reverse . unpack $ year

    auth : String
    auth = abbreviated author

    numbered : Int -> InfList String
    numbered n = Co (auth ++ yr ++ "-" ++ show n) (numbered $ n+1)

    idents : InfList String
    idents = Co (auth ++ yr) (numbered 2)

download : String -> Url -> IOE ()
download idt url = do
  prn $ "wget -O " ++ phdPapers ++ "/" ++ idt ++ ".pdf " ++ url
  result <- ioe_lift . system $ "wget -O " ++ phdPapers ++ "/" ++ idt ++ ".pdf " ++ url
  case result of
    Status 0 => return ()
    Status n => throw $ "wget: exit status " ++ show n
    Failure  => throw $ "could not execute wget"

inputEntry : IOE Entry
inputEntry = do
  prn "Put BibTeX here, end with a '.' on an empty line."
  stuff <- cat <@> inputBlock
  case stuff of
    "" => manualEntry
    _  => case parse entry stuff of
      Right e   => return e
      Left  err => throw $ "BibTeX entry not recognized:\n" ++ err

addUrl : Url -> List String -> IOE Entry
addUrl url takenIds = do
    En ty _ its' <- inputEntry

    let idt = generateId (find "author" "anon" its') (find "year" "" its') takenIds
    let its = update "url" url its'

    download idt url

    return $ En ty idt its

listEntries : List Entry -> IOE ()
listEntries = traverse_ $ prn . fmt
  where
    field : String -> List Item -> String
    field n = fromMaybe "?" . map value . Prelude.List.find (\(It k v) => k == n)

    fmt : Entry -> String
    fmt (En ty id its) = id ++ " -- " ++ field "author" its ++ " -- " ++ field "title" its

processEntries : Args -> List Entry -> IOE (List Entry)
processEntries  List     es = listEntries es >> return es
processEntries (Add url) es = (:: es) <@> addUrl url (map ident es)

usage : IOE ()
usage = prn "usage: bibdris (-a <url> | -l)"

processFile : Args -> IOE ()
processFile args = do
  stuff <- ioe_lift $ readFile phdBibtex
  case parse bibtex stuff of
    Right es => do
      es' <- processEntries args es
      ioe_lift (writeFile phdBibtex $ format es')
    Left err => throw $ "Could not parse bibtex db:\n" ++ err

main' : IOE ()
main' = ioe_lift getArgs >>= processArgs
  where
    processArgs [_, "-a", url]
      = processFile (Add url)

    processArgs [_, "-l"]
      = processFile List

    processArgs _
      = usage

main : IO ()
main = ioe_run main' (putStrLn . ("error: "++)) pure
