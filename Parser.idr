module Parser

data Tag = Lib | User
data Result str a = Success str a | Failure (List (Tag, str, String))

instance Functor (Result str) where
  map f (Success s x) = Success s (f x)
  map f (Failure es ) = Failure es

record ParserT : (m : Type -> Type) -> (str : Type) -> (a : Type) -> Type where
  PT : (runParserT : str -> m (Result str a)) -> ParserT m str a

{-
infixr 5 =<<
(=<<) : Monad m => (a -> m b) -> m a -> m b
f =<< x = x >>= f

infixr 5 <=<
(<=<) : Monad m => (b -> m c) -> (a -> m b) -> a -> m c
g <=< f = \x => (f x >>= g)
-}

fail : str -> String -> Result str a
fail s msg = Failure ((Lib, s, msg) :: [])

instance Monad m => Functor (ParserT m str) where
  map f (PT p) = PT (map (map f) . p)

instance Monad m => Applicative (ParserT m str) where
  pure x = PT (\s => pure (Success s x))
  (<$>) (PT f) (PT x) = PT (\s => f s >>= \f' => case f' of
    Failure es   => pure (Failure es)
    Success s' g => x s' >>= pure . map g)

instance Monad m => Monad (ParserT m str) where
  (>>=) (PT x) f = PT (\s => x s >>= \r => case r of
    Failure es   => pure (Failure es)
    Success s' y => let PT f' = f y in f' s')

{-
infixl 1 <??>
(<??>) : ParserT m str a -> (Tag, String) -> ParserT m str a
(PT f) <??> (t, msg) = \s => f s >>= \r => case r of
                                                -}

char : Monad m => Char -> ParserT m String ()
char c = PT f
  where
    f s with (strM s)
      f "" | StrNil = pure . fail "" $ "char '" ++ pack (c :: []) ++ "' expected, eof found"
      f (strCons x xs) | StrCons x xs = case c == x of
          True  => pure $ Success xs ()
          False => pure . fail (strCons x xs) $ "char '" ++ pack (c :: []) ++ "' expected"

--token : Monad m => String -> ParserT m String ()
