module Parser

data Result str a = Success str a | Failure String

instance Functor (Result str) where
  map f (Success s x) = Success s (f x)
  map f (Failure e  ) = Failure e

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

instance Monad m => Functor (ParserT m str) where
  map f (PT p) = PT (map (map f) . p)

instance Monad m => Applicative (ParserT m str) where
  pure x = PT (\s => pure (Success s x))
  (<$>) (PT f) (PT x) = PT (\s => f s >>= \f' => case f' of
    Failure e    => pure (Failure e)
    Success s' g => x s' >>= pure . map g)

instance Monad m => Monad (ParserT m str) where
  (>>=) {a} {b} (PT x) f = PT (\s => x s >>= cont)
    where
      cont : Result str a -> m (Result str b)
      cont (Failure e   ) = pure (Failure e)
      cont (Success s' y) = let PT f' = f y in f' s'

class Stream tok str where
  uncons : str -> Maybe (tok, str)

instance Stream Char String where
  uncons s with (strM s)
    uncons ""             | StrNil       = Nothing
    uncons (strCons x xs) | StrCons x xs = Just (x, xs)

