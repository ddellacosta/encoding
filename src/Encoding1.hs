
module Encoding1 where

import Data.Maybe
import Data.List
import Shared

toIntegerRep :: EncodedString -> Encoding -> Accumulator -> IntRep
toIntegerRep [] source acc = acc
toIntegerRep (c:cs) source acc = toIntegerRep cs source acc'
  where base = genericLength source
        acc' = (getPos c source) + (base * acc)

fromIntegerRep' :: IntRep -> Encoding -> EncodedString
fromIntegerRep' 0 target = ""
fromIntegerRep' intRep target = (getEncChar rm target) : fromIntegerRep' intRep' target
  where (intRep', rm) = divMod intRep base
        base          = genericLength target

fromIntegerRep :: IntRep -> Encoding -> EncodedString
fromIntegerRep intRep target = reverse $ fromIntegerRep' intRep target

convert :: Encoding -> Encoding -> EncodedString -> EncodedString
convert source target sourceString =
  fromIntegerRep (toIntegerRep sourceString source 0) target
