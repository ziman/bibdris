module Utils

import Control.IOExcept

%access public
%default total

data ExitStatus = Failure | Status Int

IOE : Type -> Type
IOE = IOExcept String

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

system : String -> IO ExitStatus
system cmd = translate <@> mkForeign (FFun "system" [FString] FInt) cmd
  where
    translate : Int -> ExitStatus
    translate (-1) = Failure
    translate n    = Status n

prn : String -> IOE ()
prn = ioe_lift . putStrLn
