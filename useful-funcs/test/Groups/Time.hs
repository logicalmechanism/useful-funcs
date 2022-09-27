{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module Groups.Time (tests) where

--------------------------------------------------------------------------------

import PlutusTx.Prelude

--------------------------------------------------------------------------------

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

--------------------------------------------------------------------------------

import UsefulFuncs ( lockUntilTimeInterval
                   , lockBetweenTimeInterval
                   , isTxInsideInterval
                   , isTxOutsideInterval
                   )
--------------------------------------------------------------------------------

-- test if ranges are in or outside or some other range. no crossovers intervals
prop_TimeIntervalTest = do
  { let lockRange1 = lockUntilTimeInterval   10    -- -inf to 10
  ; let lockRange2 = lockBetweenTimeInterval 1  7  
  ; let txRange1   = lockBetweenTimeInterval 3  5  
  ; let txRange2   = lockBetweenTimeInterval 13 15  
  ; let txRange3   = lockBetweenTimeInterval 5  9  
  ; let a          = isTxInsideInterval  lockRange1 txRange1       -- 3 to 5 is inside -inf to 10
  ; let b          = isTxOutsideInterval lockRange1 txRange2       -- 13 to 15 is outside -inf to 10
  ; let c          = isTxInsideInterval  lockRange2 txRange1       -- 3 to 5 is inside 1 to 7
  ; let d          = isTxOutsideInterval lockRange2 txRange2       -- 13 to 16 is outside 1 to 7
  ; let e          = not $ isTxInsideInterval  lockRange2 txRange3 -- 5 to 9 is not inside 1 to 7 and
  ; let f          = not $ isTxOutsideInterval lockRange2 txRange3 -- 5 to 9 is not outside 1 to 7
  ; all (==(True :: Bool)) [a,b,c,d,e,f]
  }


tests :: [TestTree]
tests = [ testProperty "Time Interval Test"  prop_TimeIntervalTest ]