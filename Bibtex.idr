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

infixr 3 <@>
(<@>) : Functor f => (a -> b) -> f a -> f b
f <@> x = map f x

lit : Char -> Char -> Parser String
lit l r = char l $> (map pack . many $ satisfy (/= r)) <$ char r

quotedLiteral : Parser String
quotedLiteral = lit '"' '"' <?> "quoted literal"

bracedLiteral : Parser String
bracedLiteral = lit '{' '}' <?> "braced literal"

bareWord : Parser String
bareWord = pack <@> some (satisfy isAlpha) <?> "bare word"

literal : Parser String
literal = quotedLiteral <|> bracedLiteral <|> bareWord

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
  type <- pack <@> some (satisfy (/= '{'))
  char '{'
  ident <- pack <@> some (satisfy (/= ','))
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
