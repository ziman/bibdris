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

entry : Parser Entry
entry = do
  char '@'
  type <- map pack $ some (satisfy (/= '{'))
  char '{'
  ident <- map pack $ some (satisfy (/= ','))
  char ',' $> space
  items <- item `sepBy` (char ',' $> space)
  char '}'
  space
  return $ En type ident items

bibtex : Parser (List Entry)
bibtex = space $> many entry
