module Utils

%access public

infixr 3 <@>
(<@>) : Functor f => (a -> b) -> f a -> f b
f <@> x = map f x

(>>) : Monad m => m a -> m b -> m b
x >> y = x >>= \_ => y

writeFile : String -> String -> IO ()
writeFile fn stuff = do
  f <- openFile fn Write
  fwrite f stuff
  closeFile f

cat : (Foldable f, Monoid a) => f a -> a
cat = concat
