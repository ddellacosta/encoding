{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Encoding4 where

import Control.Arrow
import Data.List
import Data.Monoid
import Shared

data ListF a b = Nil | Cons a b deriving (Functor, Show)


-- fold / cata-/para-morphism

project :: [a] -> ListF a [a]
project = \case
    [] -> Nil
    x:xs -> Cons x xs

cata :: (ListF a b -> b) -> [a] -> b
cata f = project >>> fmap (cata f) >>> f

para :: (ListF a ([a], b) -> b) -> [a] -> b
para rAlg = project >>> fmap fanout >>> rAlg
    where fanout t = (t, para rAlg t)

-- para (toIntegerRep' "01") "1010"
-- > 10

-- alternate implementation not requiring reverse
toIntegerRep' :: Encoding -> (ListF Char ([Char], IntRep)) -> IntRep
toIntegerRep' _ Nil = 0
toIntegerRep' source (Cons c (rem, acc)) = 
  acc + (base ^ (genericLength rem) * (getPos c source))
  where base = genericLength source

-- requires a reverse
toIntegerRep :: Encoding -> (ListF Char IntRep) -> IntRep
toIntegerRep source Nil = 0
toIntegerRep source (Cons c acc) = (getPos c source) + (base * acc)
  where base = genericLength source

 
-- unfold / apomorphism

embed :: ListF a [a] -> [a]
embed = \case
  Nil -> []
  Cons a as -> a : as

ana :: (b -> ListF a b) -> b -> [a]
ana f = embed <<< fmap (ana f) <<< f

-- reverse $ ana (fromIntegerRep "01") 10
-- > "1010"

fromIntegerRep :: Encoding -> IntRep -> ListF Char IntRep 
fromIntegerRep target 0 = Nil
fromIntegerRep target n =
  let (intRep', rm) = divMod n base
      base = genericLength target
  in Cons (getEncChar rm target) intRep'

convert :: Encoding -> Encoding -> EncodedString -> EncodedString
convert source target = 
  reverse >>> cata (toIntegerRep source) >>> ana (fromIntegerRep target) >>> reverse
