
module Encoding2 where

import Control.Monad.Fix
import Data.List
import Shared

--
-- (from Control.Monad.Fix)
--
-- fix :: (a -> a) -> a
-- fix f = let {x = f x} in x

--
-- > fix toIntegerRep "1010" "01" 0
-- 10
--

toIntegerRep :: (EncodedString -> Encoding -> Accumulator -> IntRep)
  -> EncodedString -> Encoding -> Accumulator -> IntRep
toIntegerRep _ [] source acc = acc
toIntegerRep f (c:cs) source acc = f cs source acc'
  where base = genericLength source
        acc' = (getPos c source) + (base * acc)


--
-- (still needs the final reversal)
--
-- > fix fromIntegerRep' 10 "01"
-- "0101"
--

fromIntegerRep' :: (IntRep -> Encoding -> EncodedString)
  -> IntRep -> Encoding -> EncodedString
fromIntegerRep' _ 0 target = ""
fromIntegerRep' f intRep target = (getEncChar rm target) : f intRep' target
  where (intRep', rm) = divMod intRep base
        base          = genericLength target

fromIntegerRep :: IntRep -> Encoding -> EncodedString
fromIntegerRep intRep target = reverse $ fix fromIntegerRep' intRep target

convert :: Encoding -> Encoding -> EncodedString -> EncodedString
convert source target sourceString = fromIntegerRep intRep target
  where intRep = (fix toIntegerRep sourceString source 0)
