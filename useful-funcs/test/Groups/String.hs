{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module Groups.String (tests) where

--------------------------------------------------------------------------------

import PlutusTx.Prelude

--------------------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.QuickCheck

--------------------------------------------------------------------------------

import StringFuncs ( hash
                   , byteStringAsIntegerList
                   , createBuiltinByteString
                   , integerAsByteString
                   , convertByteStringToInteger
                   , convertToString
                   )

import MathFuncs   ( baseQ
                   , pow
                   )
--------------------------------------------------------------------------------

-- build a string a check the hash
prop_ConvertToString = do
  { let a = convertToString [0..15] "" == "0123456789abcdef"
  ; let b = convertToString (baseQ (pow 3 334) 64) "" == "2ChYZcTlhHNbjZnxX4PFzuN5lg7Aqv4FipthKFSBx/Evkbw7vQ2mYhvUB2iWZtdN1yw2oOKkRrYALLzWmeIOmUGhV"
  ; let c = convertToString [65] "" == ""
  ; let d = convertToString [] "" == ""
  ; all (==(True :: Bool)) [a,b,c,d]
  }

-- build a string a check the hash
prop_ByteStringManipulationTest = do
  { let hexHash = "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a" -- hardcoded bytestring
  ; let a = hash "" == (createBuiltinByteString $ byteStringAsIntegerList hexHash)
  ; all (==(True :: Bool)) [a]
  }

-- integer to string
prop_IntegerToStringTest = do
  { let a = integerAsByteString 1234567890       == "1234567890"
  ; let b = integerAsByteString 0                == "0"
  ; let c = "Testing_" <> integerAsByteString 42 == "Testing_42"
  ; all (==(True :: Bool)) [a,b,c]
  }

-- convert a string into a number via product
prop_ConvertByteStringToIntegerTest = do
  { let a = (convertByteStringToInteger $ hash "")     == 2739796737785856
  ; let b = convertByteStringToInteger ""              == 0
  ; let c = (convertByteStringToInteger $ hash "test") == 4622965172100
  ; let d = convertByteStringToInteger "acab"          == 0
  ; let e = convertByteStringToInteger "acabef12"      == 49945313880000
  ; all (==(True :: Bool)) [a,b,c,d,e]
  }


tests :: [TestTree]
tests = [ testProperty "Manipulate ByteStrings"  prop_ByteStringManipulationTest
        , testProperty "Write Integer As String" prop_IntegerToStringTest
        , testProperty "Convert String To Int"   prop_ConvertByteStringToIntegerTest
        , testProperty "Convert [Int] To String" prop_ConvertToString
        ]