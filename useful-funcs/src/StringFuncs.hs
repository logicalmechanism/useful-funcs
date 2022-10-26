{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
{-|
Module      : StringFuncs
Description : A Collection of String Functions
Copyright   : (c) Ancient Kraken, 2022
License     : GPL-3
Maintainer  : logical.mechanism@protonmail.com
Stability   : stable

A collection of tested on and off chain plutus string functions.
-}
module StringFuncs
  ( byteStringAsIntegerList
  , integerAsByteString
  , hash
  , createBuiltinByteString
  , convertByteStringToInteger
  , convertToString
  ) where
import PlutusTx.Prelude
import Plutus.V2.Ledger.Api        as V2
import MathFuncs                   ( pow, baseQ )
-------------------------------------------------------------------------------
-- | The Mapping for converting an integer into a stringed version.
-- This works well for base 64 or less.
--
-- Not Exposed
-------------------------------------------------------------------------------
integerToStringMapping :: Integer -> V2.BuiltinByteString
integerToStringMapping ch
  | ch == 0   = "0"
  | ch == 1   = "1"
  | ch == 2   = "2"
  | ch == 3   = "3"
  | ch == 4   = "4"
  | ch == 5   = "5"
  | ch == 6   = "6"
  | ch == 7   = "7"
  | ch == 8   = "8"
  | ch == 9   = "9"
  | ch == 10  = "a"
  | ch == 11  = "b"
  | ch == 12  = "c"
  | ch == 13  = "d"
  | ch == 14  = "e"
  | ch == 15  = "f"
  | ch == 16  = "g"
  | ch == 17  = "h"
  | ch == 18  = "i"
  | ch == 19  = "j"
  | ch == 20  = "k"
  | ch == 21  = "l"
  | ch == 22  = "m"
  | ch == 23  = "n"
  | ch == 24  = "o"
  | ch == 25  = "p"
  | ch == 26  = "q"
  | ch == 27  = "r"
  | ch == 28  = "s"
  | ch == 29  = "t"
  | ch == 30  = "u"
  | ch == 31  = "v"
  | ch == 32  = "w"
  | ch == 33  = "x"
  | ch == 34  = "y"
  | ch == 35  = "z"
  | ch == 36  = "A"
  | ch == 37  = "B"
  | ch == 38  = "C"
  | ch == 39  = "D"
  | ch == 40  = "E"
  | ch == 41  = "F"
  | ch == 42  = "G"
  | ch == 43  = "H"
  | ch == 44  = "I"
  | ch == 45  = "J"
  | ch == 46  = "K"
  | ch == 47  = "L"
  | ch == 48  = "M"
  | ch == 49  = "N"
  | ch == 50  = "O"
  | ch == 51  = "P"
  | ch == 52  = "Q"
  | ch == 53  = "R"
  | ch == 54  = "S"
  | ch == 55  = "T"
  | ch == 56  = "U"
  | ch == 57  = "V"
  | ch == 58  = "W"
  | ch == 59  = "X"
  | ch == 60  = "Y"
  | ch == 61  = "Z"
  | ch == 62  = "+"
  | ch == 63  = "/"
  | otherwise = emptyByteString
-------------------------------------------------------------------------------
-- | The Mapping for converting an integer into a stringed version.
-- Not Exposed
-------------------------------------------------------------------------------
stringToIntegerMapping :: V2.BuiltinByteString -> Integer
stringToIntegerMapping ch
  | ch == "0" = 0
  | ch == "1" = 1
  | ch == "2" = 2
  | ch == "3" = 3
  | ch == "4" = 4
  | ch == "5" = 5
  | ch == "6" = 6
  | ch == "7" = 7
  | ch == "8" = 8
  | ch == "9" = 9
  | ch == "a" = 10
  | ch == "b" = 11
  | ch == "c" = 12
  | ch == "d" = 13
  | ch == "e" = 14
  | ch == "f" = 15
  | otherwise = 0
-------------------------------------------------------------------------------
-- | Converts a hex encoded string into a list of integers for hardcoding. Without
-- type validation there is not way to hardcode bytestring unless they are represented
-- as a list of integers that can be mapped back into a bytestring on-chain.
--
-- 
-- >>> pkh = "31ec74a9f86884e7a16f8fc30840f7f409c08b91e93d2be3a4377442"
-- >>> byteStringAsIntegerList pkh
-- [49,236,116,169,248,104,132,231,161,111,143,195,8,64,247,244,9,192,139,145,233,61,43,227,164,55,116,66]
--
--
-- Testing: Test.Groups.String
-------------------------------------------------------------------------------
byteStringAsIntegerList :: V2.BuiltinByteString -> [Integer]
byteStringAsIntegerList str' = createList str' 0 []
  where
    length' :: Integer
    length' = divide (lengthOfByteString str') 2

    createList :: V2.BuiltinByteString -> Integer -> [Integer] -> [Integer]
    createList str counter value' =
      if counter >= length'
        then value'
        else createList (dropByteString 2 str) (counter+1) (value' <> [convertNumber (takeByteString 2 str) 0 1])
    
    convertNumber :: V2.BuiltinByteString -> Integer -> Integer -> Integer
    convertNumber nList value counter =
      if counter < 0
        then value
        else convertNumber (dropByteString 1 nList) (value + (pow 16 counter * val') ) (counter - 1)
      where
        first' :: V2.BuiltinByteString
        first' = takeByteString 1 nList

        val' :: Integer
        val' = stringToIntegerMapping first'
-------------------------------------------------------------------------------
-- | Convert an integer into a string. This converts an integer into a list of
-- integers representing the digit in base 10. This list is feed into a mapping
-- that converts the integer into a bytestring. Great for creating incremental
-- token names.
--
-- >>> "HelloWorld_" <> integerAsByteString 42
-- "HelloWorld_42"
--
-- Testing: Test.Groups.String
-------------------------------------------------------------------------------
integerAsByteString :: Integer -> V2.BuiltinByteString
integerAsByteString num = 
  if num == 0 
    then "0" 
    else 
      if num > 0
        then convertToString (base10 num) "" 
        else "-" <> convertToString (base10 (-1*num)) "" -- attach the negative sign
  where
    base10 :: Integer -> [Integer]
    base10 num' = baseQ num' 10
-------------------------------------------------------------------------------
-- | Converts a list of integers into a string by mapping integers to letters up
-- to the value of 64. This is unlike createBuiltinByteString which uses ascii codes
-- to create a string where this is a one to one mapping.
--
-- >>> convertToString [0..64] ""
-- "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+/"
--
-- Testing: Test.Groups.String
-------------------------------------------------------------------------------
convertToString :: [Integer] -> BuiltinByteString -> BuiltinByteString
convertToString []     str = str
convertToString (x:xs) str = convertToString xs (str <> integerToStringMapping x)
-------------------------------------------------------------------------------
-- | Compute the sha3_256 hash of some bytestring.
--
-- >>> hash "Hello, World!"
-- "\SUB\241zfN?\168\228\EM\184\186\ENQ\194\161s\SYN\157\247ab\165\162\134\224\196\ENQ\180`\212x\247\239"
-- >>> DatumHash $ hash "Hello, World!"
-- 1af17a664e3fa8e419b8ba05c2a173169df76162a5a286e0c405b460d478f7ef
--
-- Testing: Test.Groups.String
-------------------------------------------------------------------------------
hash :: V2.BuiltinByteString -> V2.BuiltinByteString
hash string = sha3_256 string
-------------------------------------------------------------------------
-- | Creates a proper BuiltinByteString type from a list of integers. This
-- is a hack solution to by pass the loss of type validation. It should be
-- used with `byteStringAsIntegerList`. This allows a bytestring to be hard
-- coded into a contract at compile time.
--
-- @
-- createBuiltinByteString $ byteStringAsIntegerList pkh
-- @
--
-- Testing: Test.Groups.String
-------------------------------------------------------------------------
createBuiltinByteString :: [Integer] -> V2.BuiltinByteString
createBuiltinByteString intList = flattenBuiltinByteString [ consByteString x emptyByteString | x <- intList]
  where
    flattenBuiltinByteString :: [V2.BuiltinByteString] -> V2.BuiltinByteString
    flattenBuiltinByteString []     = emptyByteString 
    flattenBuiltinByteString (x:xs) = appendByteString x (flattenBuiltinByteString xs)
-------------------------------------------------------------------------
-- | Take in a bytestring and converts it to a number via a product. Similar
-- to creating an integer list but instead takes the product of all the values.
-- The product is limited to numbers less than 2^64 - 1.
--
-- Testing: Test.Groups.String
-------------------------------------------------------------------------
convertByteStringToInteger :: BuiltinByteString -> Integer
convertByteStringToInteger hexString = 
  if hexString == emptyByteString
    then 0
    else 
      if lengthOfByteString hexString >= fixedLength + 2
        then hexStringToInteger hexString fixedLength 1 -- force length
        else 0
  where
    -- this will restrict to numbers less than 2^64 - 1
    fixedLength :: Integer
    fixedLength = 6

    -- add 1 to each number to avoid multiplying by zero
    hexStringToInteger :: BuiltinByteString -> Integer -> Integer -> Integer
    hexStringToInteger hex_string counter value'
      | counter > 0 = hexStringToInteger hex_string (counter - 1) (value' * (indexByteString hex_string counter + 1))
      | otherwise = value' * (indexByteString hex_string 0 + 1)
