module Week04.MaybeNotes where

import Text.Read (readMaybe)
import Week04.MonadNotes

foo :: String -> String -> String -> Maybe Int
-- we want to try to pass all 3 strings as ints, and if this is successful we add the ints,
-- but if one fails then we want to return nothing.
foo x y z = case readMaybe x of
  Nothing -> Nothing
  Just k  -> case readMaybe y of
    Nothing -> Nothing
    Just l  -> case readMaybe z of
      Nothing -> Nothing
      Just m -> Just (k + l + m)

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _  = Nothing
bindMaybe (Just x) f = f x

foo' :: String -> String -> String -> Maybe Int
foo' x y z = readMaybe x `bindMaybe` \k ->
             readMaybe y `bindMaybe` \l ->
             readMaybe z `bindMaybe` \m ->
             Just (k + l + m)

foo'' :: String -> String -> String -> Maybe Int
foo'' x y z = threeInts (readMaybe x) (readMaybe y) (readMaybe z)