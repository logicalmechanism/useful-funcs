{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module Groups.Math (tests) where

--------------------------------------------------------------------------------

import PlutusTx.Prelude

--------------------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.QuickCheck

--------------------------------------------------------------------------------

import MathFuncs ( logOfXInBaseB
                 , baseQ
                 , pow
                 , powmod
                 , percentage
                 , isIntegerInRange
                 )
--------------------------------------------------------------------------------

-- log stuff
prop_LogTest = do
  { let a = logOfXInBaseB 10 0 == 0
  ; let b = logOfXInBaseB 0  2 == 0
  ; let c = logOfXInBaseB 2  2 == 1
  ; let d = logOfXInBaseB 89 3 == 4
  ; all (==(True :: Bool)) [a,b,c,d]
  }

-- write n in base q
prop_BaseQTest = do
  { let a = baseQ 46342 0  == []
  ; let b = baseQ 0     3  == []
  ; let c = baseQ 0     0  == []
  ; let d = baseQ 46342 17 == [9,7,6,0]
  ; all (==(True :: Bool)) [a,b,c,d]
  }

-- pow stuff
prop_PowTest = do
  { let a = pow 10 0 == 1
  ; let b = pow 0  2 == 0
  ; let c = pow 2  2 == 4
  ; let d = pow 89 3 == 704969
  ; all (==(True :: Bool)) [a,b,c,d]
  }

-- power mod stuff
prop_PowModTest = do
  { let a = powmod 10 0 0 == 0
  ; let b = powmod 0  2 0 == 0
  ; let c = powmod 2  2 3 == 1
  ; let d = powmod 89 3 52 == 5
  ; all (==(True :: Bool)) [a,b,c,d]
  }

-- percentage stuff
prop_PercentageTest = do
  { let a = percentage 1           0   ==  0
  ; let b = percentage 100000000   40  ==  2500000
  ; let c = percentage 100000000 (-40) == -2500000
  ; let d = percentage 10          100 ==  0
  ; all (==(True :: Bool)) [a,b,c,d]
  }

-- integer in range
prop_IsIntegerInRangeTest = do
  { let a = isIntegerInRange 100 40 99 == True
  ; let b = isIntegerInRange 100 40 89 == False
  ; let c = isIntegerInRange 100 0  89 == False
  ; let d = isIntegerInRange 0   0  0  == True
  ; all (==(True :: Bool)) [a,b,c,d]
  }

tests :: [TestTree]
tests = [ testProperty "Log Of x In base b"      prop_LogTest
        , testProperty "Write N In Base Q"       prop_BaseQTest
        , testProperty "x To The Power Of n"     prop_PowTest
        , testProperty "x To The Power Of n % q" prop_PowModTest
        , testProperty "Percentage of some n"    prop_PercentageTest
        , testProperty "is int in some range"    prop_IsIntegerInRangeTest
        ]