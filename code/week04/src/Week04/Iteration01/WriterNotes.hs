module Week04.WriterNotes where

import Week04.MonadNotes
import Control.Monad -- we import to implement Monad instances

data Writer a = Writer a [String]
  deriving Show

number :: Int -> Writer Int
number n = Writer n $ ["number: " ++ show n] -- value and list of log messages

tell :: [String] -> Writer () -- only cares about log messages
tell = Writer ()

foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo (Writer k xs) (Writer l ys) (Writer m zs) =
  let -- includes sum into log messages
    s = k + l + m
    Writer _ us = tell ["sum: " ++ show s]
  in
    Writer (k + l + m) $ xs ++ ys ++ zs ++ us

bindWriter :: Writer a -> (a -> Writer b) -> Writer b
bindWriter (Writer a xs) f =
  let
    Writer b ys = f a
  in
    Writer b $ xs ++ ys

foo' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo' x y z = x `bindWriter` \k ->
            y `bindWriter` \l ->
            z `bindWriter` \m ->
            let s = k + l + m
            in tell ["sum " ++ show s] `bindWriter` \_ ->
              Writer s []

foo'' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo'' x y z = threeInts x y z          >>= \s -> -- binds the non-side-effected result to s
              tell ["sum: " ++ show s] >> -- skips the side-effects and goes straight to returning the [String]
              return s

instance Functor Writer where
  fmap = liftM --liftM implements fmap from bind
instance Applicative Writer where
  pure = return
  (<*>) = ap   -- helps implement return and fmap
instance Monad Writer where
  return a = Writer a []
  (>>=) = bindWriter

--once Monad instances are defined, we can use monad functions
