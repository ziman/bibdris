module Bibtex

import Control.Monad.Identity

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.String_

record Item : Type where
  It
    :  (name : String)
    -> (value : String)
    -> Item

record Entry : Type where
  En
    :  (type : String)
    -> (ident : String)
    -> (items : List Item)
    -> Entry

Parser : Type -> Type
Parser = ParserT Identity String

lit : Char -> Char -> Parser String
lit l r = char l $> (map pack . many $ satisfy (/= r)) <$ char r

quotedLiteral : Parser String
quotedLiteral = lit '"' '"'

bracedLiteral : Parser String
bracedLiteral = lit '{' '}'

literal : Parser String
literal = quotedLiteral <|> bracedLiteral

item : Parser Item
item = do
  name <- literal
  space
  char '='
  space
  value <- literal
  space
  return $ It name value

comma : Parser ()
comma = char ',' <$ space

entry : Parser Entry
entry = do
  char '@'
  type <- map pack $ some (satisfy (/= '{'))
  char '{'
  ident <- map pack $ some (satisfy (/= ','))
  char ','
  space
  items <- item `sepBy` comma
  char '}'
  space
  return $ En type ident items

bibtex : Parser (List Entry)
bibtex = space $> many entry

quote : String -> String
quote s = "\"" ++ s ++ "\""

brace : String -> String
brace s = "{" ++ s ++ "}"

cmap : (a -> String) -> List a -> String
cmap f []        = ""
cmap f (s :: ss) = f s ++ cmap f ss

instance Show Item where
  show (It n v) = quote n ++ " = " ++ brace v

instance Show Entry where
  show (En ty id xs) = "@" ++ id ++ "{\n" ++ cmap (("  " ++) . (++ "\n") . show) xs ++ "\n}"

runParser : Parser a -> String -> Result String a
runParser (PT f) s = let Id p = f s in p
