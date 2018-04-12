{-# LANGUAGE LambdaCase #-}

module Encoding3 where

import Data.Bifunctor
import Data.List
import Data.Tuple
import Shared

toIntegerRep :: EncodedString -> Encoding -> IntRep
toIntegerRep es source = foldl' (\acc c -> (getPos c source) + (base * acc)) 0 es 
  where base = genericLength source

fromIntegerRep :: IntRep -> Encoding -> EncodedString
fromIntegerRep intRep target = reverse $ unfoldr
  (\case
      0 -> Nothing
      n -> let (intRep', rm) = divMod n base
           in Just ((getEncChar rm target), intRep'))
  intRep
  where base = genericLength target

convert :: Encoding -> Encoding -> EncodedString -> EncodedString
convert source target = (flip fromIntegerRep target) . (flip toIntegerRep source)


-- a slightly cleaner approach, using bifunctor to avoid using let

fromIntegerRep2 :: IntRep -> Encoding -> EncodedString
fromIntegerRep2 intRep target =
  let base = genericLength target
      go 0 = Nothing
      go n = (Just . swap . second (flip getEncChar target)) (divMod n base)
  in reverse $ unfoldr go intRep
