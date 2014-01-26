module Main

import System
import Control.Catchable

import Bibtex
import Utils

import Data.Text
import Data.Text.IO
import Lightyear.Text

Url : Type
Url = Text

data Args = List | Add Url

data InfList : Type -> Type where
  Co : (x : a) -> |(xs : InfList a) -> InfList a

phdRoot : String
phdRoot = "/home/ziman/phd"

phdBibtex : String
phdBibtex = phdRoot ++ "/db.bib"

phdPapers : String
phdPapers = phdRoot ++ "/papers"

inputBlock : IOE (List Text)
inputBlock = do
  ln <- trim <@> ioe_lift getLine
  if ln == str "."
     then return []
     else (ln ::) <@> inputBlock

askFor : Text -> IOE (List Item -> List Item)
askFor k = do
  prn $ k ++ str ":"
  v <- trim <@> ioe_lift getLine
  return $ if null v
    then id
    else (It k v ::)

manualEntry : IOE Entry
manualEntry = do
    prn $ str "Switching to manual entry:"
    ty <- do
      prn $ str "type [article]:"
      ty <- trim <@> ioe_lift getLine
      return $ if null ty
        then str "article"
        else ty

    its <- foldr ($) Prelude.List.Nil <@> traverse askFor requiredItems
    return $ En ty (str "no-id-yet") its
  where
    requiredItems : List Text
    requiredItems = [str "title", str "author", str "year"]

abbreviated : Text -> Text
abbreviated txt = pack . map toLower . abbreviate $ surnames
  where
    authors : List Text
    authors = split (== ',') txt

    names : List (List Text)
    names = map words authors

    surname : List Text -> Text
    surname []  = "unknown"
    surname [x] = x
    surname (x :: xs) = surname xs

    surnames : List Text
    surnames = map surname names

    firstLetter : Text -> CodePoint
    firstLetter n with (head n)
      | Nothing = fromChar '_'
      | Just c  = c

    abbreviate : (surnames : List Text) -> List CodePoint
    abbreviate []  = unpack "unknown"
    abbreviate [s] = unpack s
    abbreviate ss  = map firstLetter ss

first : (a -> Bool) -> InfList a -> a
first p (Co x xs) with (p x)
  | True  = x
  | False = first p xs

generateId : Text -> Text -> List Text -> Text
generateId author year taken = first (\x => not $ elem x taken) idents
  where
    yr : Text
    yr = reverse . take 2 . reverse $ year

    auth : Text
    auth = abbreviated author

    numbered : Int -> InfList Text
    numbered n = Co (auth ++ yr ++ "-" ++ show n) (numbered $ n+1)

    idents : InfList Text
    idents = Co (auth ++ yr) (numbered 2)

download : Text -> Url -> IOE ()
download idt url = do
    result <- sys $ str "wget -O " ++ phdPapers ++ str "/" ++ idt ++ str ".pdf " ++ url
    case result of
      Status 0 => return ()
      Status n => throw $ "wget: exit status " ++ show n
      Failure  => throw $ "could not execute wget"
  where
    sys : Text -> IOE ExitStatus
    sys = ioe_lift . system . toString . getBytes 

inputEntry : IOE Entry
inputEntry = do
  prn $ str "Put BibTeX here, end with a '.' on an empty line."
  stuff <- cat <@> inputBlock
  if null stuff
    then manualEntry
    else case parse entry stuff of
      Right e   => return e
      Left  err => throw $ "BibTeX entry not recognized:\n" ++ err

addUrl : Url -> List Text -> IOE Entry
addUrl url takenIds = do
    En ty _ its' <- inputEntry

    let idt = generateId (find (str "author") (str "anon") its') (find (str "year") empty its') takenIds
    let its = update (str "url") url its'

    download idt url

    return $ En ty idt its

listEntries : List Entry -> IOE ()
listEntries = traverse_ $ prn . fmt
  where
    field : Text -> List Item -> Text
    field n = fromMaybe (str "?") . map value . Prelude.List.find (\(It k v) => k == n)

    fmt : Entry -> Text
    fmt (En ty id its) = id ++ str " -- " ++ field (str "author") its ++ " -- " ++ field (str "title") its

processEntries : Args -> List Entry -> IOE (List Entry)
processEntries  List     es = listEntries es >> return es
processEntries (Add url) es = (:: es) <@> addUrl url (map ident es)

usage : IOE ()
usage = prn $ str "usage: bibdris (-a <url> | -l)"

processFile : Args -> IOE ()
processFile args = do
  stuff <- ioe_lift $ readFile phdBibtex
  case parse bibtex stuff of
    Right es => do
      es' <- processEntries args es
      ioe_lift (writeFile phdBibtex UTF8 $ format es')
    Left err => throw $ "Could not parse bibtex db:\n" ++ err

main' : IOE ()
main' = ioe_lift getArgs >>= processArgs
  where
    processArgs [_, "-a", url]
      = processFile (Add $ fromString url `asEncodedIn` UTF8)

    processArgs [_, "-l"]
      = processFile List

    processArgs _
      = usage

main : IO ()
main = ioe_run main' (putStrLn . ("error: "++)) pure
