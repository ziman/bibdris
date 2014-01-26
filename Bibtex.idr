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
lit l r = char l $> (map Data.Text.pack . many $ satisfy (/= fromChar r)) <$ char r

token : String -> Parser ()
token s = ascii s <$ space

quotedLiteral : Parser Text
quotedLiteral = lit '"' '"' <?> "quoted literal"

bracedLiteral : Int -> Parser Text
bracedLiteral n = do
    char '{'
    strings <- alternating unbraced $ bracedLiteral (n+1)
    char '}'
    return $ case n of
      0 =>        cat strings
      _ => "{" ++ cat strings ++ "}"
  where
    unbraced : Parser Text
    unbraced = Data.Text.pack <@> many (satisfy $ \x => x /= fromChar '{' && x /= fromChar '}')

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
bibtex = space $> many entry

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
