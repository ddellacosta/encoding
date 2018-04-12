
module Encoding5 where

import Control.Arrow
import Data.Functor.Foldable
import Data.List
import Shared

toIntegerRep :: Encoding -> (ListF Char IntRep) -> IntRep
toIntegerRep source Nil = 0
toIntegerRep source (Cons c acc) = (getPos c source) + (base * acc)
  where base = genericLength source

fromIntegerRep :: Encoding -> IntRep -> ListF Char IntRep 
fromIntegerRep target 0 = Nil
fromIntegerRep target n =
  let (intRep', rm) = divMod n base
      base = genericLength target
  in Cons (getEncChar rm target) intRep'

convert :: Encoding -> Encoding -> EncodedString -> EncodedString
convert source target =
  reverse >>> cata (toIntegerRep source) >>> ana (fromIntegerRep target) >>> reverse
