module Treb.Test.Utils where

filterMaybe :: (a -> Bool) -> a -> Maybe a
filterMaybe p x =
  if p x then
    Just x
  else
    Nothing
