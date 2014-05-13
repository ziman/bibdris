module Bibtex

import Control.Monad.Identity

import Data.Text

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Text

import Utils

record Item : Type where
  It
    :  (name : Text)
    -> (value : Text)
    -> Item

record Entry : Type where
  En
    :  (type : Text)
    -> (ident : Text)
    -> (items : List Item)
    -> Entry

lit : Char -> Char -> Parser Text
lit l r = char l $> content <$ char r
  where
    content : Parser Text
    content = map Data.Text.pack . many $ satisfy (/= fromChar r)

-- This is a hint for the elaborator
-- in order to resolve typeclasses properly.
-- The culprit are probably insufficient constraints on encoding,
-- in Lightyear.Text, which makes encoding impossible to infer.
-- By forcing the type to Parser, we also force the encoding to be UTF-8.
elabHint : Parser a -> Parser a
elabHint = id

token : String -> Parser ()
token s = ascii s $> elabHint space

quotedLiteral : Parser Text
quotedLiteral = lit '"' '"' <?> "quoted literal"

bracedLiteral : Int -> Parser Text
bracedLiteral n = do
    strings <- char '{' $>| alternating unbraced (bracedLiteral (n+1)) <$| char '}'
    return $ if n == 0
      then cat strings
      else str "{" ++ cat strings ++ str "}"
  where
    nonBrace : Parser CodePoint
    nonBrace = satisfy (\x => x /= fromChar '{' && x /= fromChar '}')

    unbraced : Parser Text
    unbraced = Data.Text.pack <@> many nonBrace

bareWord : Parser Text
bareWord = Data.Text.pack <@> some (satisfy isAlpha) <?> "bare word"

literal : Parser Text
literal = (quotedLiteral <|> bracedLiteral 0 <|> bareWord) <$ space

item : Parser Item
item = do
  name <- literal
  token "="
  value <- literal
  return $ It name value

entry : Parser Entry
entry = do
  token "@"
  type <- Data.Text.pack <@> some (satisfy (/= fromChar '{'))
  token "{"
  ident <- Data.Text.pack <@> some (satisfy (/= fromChar ','))
  token ","
  items <- item `sepBy` token ","
  token "}"
  return $ En type ident items

bibtex : Parser (List Entry)
bibtex = elabHint space $> many entry

quote : Text -> Text
quote s = str "\"" ++ s ++ str "\""

brace : Text -> Text
brace s = str "{" ++ s ++ str "}"

unitems : Text -> Text -> List Text -> Text
unitems pre sep       []  = empty
unitems pre sep (x :: []) = pre ++ x
unitems pre sep (x :: xs) = pre ++ x ++ sep ++ unitems pre sep xs

showItem : Item -> Text
showItem (It n v) = n ++ str " = " ++ brace v

showEntry : Entry -> Text
showEntry (En ty id xs) = str "@" ++ ty ++ str "{" ++ id ++ str ",\n"
    ++ (unitems (str "  ") (str ",\n") . map showItem) xs
    ++ str "\n}\n"

format : List Entry -> Text
format = unitems (str "") (str "\n") . map showEntry

update : Text -> Text -> List Item -> List Item
update k v [] = It k v :: []
update k v (It k' v' :: xs) with (k == k')
  | True  = It k  v  :: xs
  | False = It k' v' :: update k v xs

find : Text -> Text -> List Item -> Text
find k def [] = def
find k def (It k' v' :: xs) with (k == k')
  | True  = v'
  | False = find k def xs
