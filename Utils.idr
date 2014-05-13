module Utils

import Data.Text
import Data.Text.IO
import Control.IOExcept

%access public
%default total

data ExitStatus = Failure | Status Int

IOE : Type -> Type
IOE = IOExcept String

(<@>) : Functor f => (a -> b) -> f a -> f b
f <@> x = map f x

(>>) : Monad m => m a -> m b -> m b
x >> y = x >>= \_ => y

cat : (Foldable f, Monoid a) => f a -> a
cat = concat

system : String -> IO ExitStatus
system cmd = translate <@> mkForeign (FFun "system" [FString] FInt) cmd
  where
    translate : Int -> ExitStatus
    translate (-1) = Failure
    translate n    = Status n

prn : Text -> IOE ()
prn = ioe_lift . putStrLn

-- TODO: move this to Data.Text
trim : Text -> Text
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
