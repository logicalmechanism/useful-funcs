{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module Groups.Helpers (tests) where

--------------------------------------------------------------------------------

import PlutusTx.Prelude

--------------------------------------------------------------------------------

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

--------------------------------------------------------------------------------

import UsefulFuncs ( replicate
                   , logOfXInBaseB
                   , hash
                   , byteStringAsIntegerList
                   , createBuiltinByteString
                   , integerAsByteString
                   , baseQ
                   )
--------------------------------------------------------------------------------

-- replicate list stuff
prop_ReplicateTest = do
  { let a = replicate [1,2,3] 2 == [1,2,3,1,2,3]
  ; let b = replicate []      2 == []
  ; let c = replicate [1,2,3] 0 == []
  ; all (==(True :: Bool)) [a,b,c]
  }

-- log stuff
prop_LogTest = do
  { let a = logOfXInBaseB 10 0 == 0
  ; let b = logOfXInBaseB 0  2 == 0
  ; let c = logOfXInBaseB 2  2 == 1
  ; let d = logOfXInBaseB 89 3 == 4
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
  { let a = integerAsByteString 1234567890 == "1234567890"
  ; let b = integerAsByteString 0          == "0"
  ; all (==(True :: Bool)) [a,b]
  }

-- write n in base q
prop_BaseQTest = do
  { let a = baseQ 46342 0  == []
  ; let b = baseQ 0     3  == []
  ; let c = baseQ 0     0  == []
  ; let d = baseQ 46342 17 == [9,7,6,0]
  ; all (==(True :: Bool)) [a,b,c,d]
  }


tests :: [TestTree]
tests = [ testProperty "Replicate List N Times"  prop_ReplicateTest
        , testProperty "Log Of x In base b"      prop_LogTest
        , testProperty "Manipulate ByteStrings"  prop_ByteStringManipulationTest
        , testProperty "Write Integer As String" prop_IntegerToStringTest
        , testProperty "Write N In Base Q"       prop_BaseQTest
        ]