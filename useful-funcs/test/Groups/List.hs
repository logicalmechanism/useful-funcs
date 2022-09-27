{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module Groups.List (tests) where

--------------------------------------------------------------------------------

import PlutusTx.Prelude
import Plutus.V1.Ledger.Address
import Plutus.V2.Ledger.Api        as V2
import Plutus.V2.Ledger.Contexts   as V2
import Plutus.V1.Ledger.Value      as Value
import Plutus.V1.Ledger.Time       as Time
import Plutus.V1.Ledger.Interval   as Interval
import PlutusTx.Builtins.Internal (mkI)

--------------------------------------------------------------------------------

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

--------------------------------------------------------------------------------

import UsefulFuncs ( addTwoLists
                   , subTwoLists
                   , multiplyTwoLists
                   , multiplyAList
                   , divideTwoLists
                   , divideAList
                   )
--------------------------------------------------------------------------------

-- add two list together element by element to create a new list
prop_AddListTest = do 
  { let a = addTwoLists [1,2,3]  [3,2,1]   == [4,4,4]
  ; let b = addTwoLists []       []        == []
  ; let c = addTwoLists [1,2,3]  []        == []
  ; let d = addTwoLists []       [1,2,3]   == []
  ; let e = addTwoLists [1,2,3]  [1,2,3,4] == [2,4,6]
  ; all (==(True :: Bool)) [a,b,c,d,e]
  }

-- subtract two list together element by element to create a new list
prop_SubListTest = do
  { let a = subTwoLists [3,2,1] [1,2,3]   == [2,0,0]
  ; let b = subTwoLists [1,2,3] []        == []
  ; let c = subTwoLists [5,4,3] [1,2,3,4] == [4,2,0]
  ; let d = subTwoLists []      []        == []
  ; let e = subTwoLists []      [1,2,3]   == []
  ; all (==(True :: Bool)) [a,b,c,d,e]
  }

-- multiply two list together element by element to create a new list
prop_MultiplyTwoListTest = do
  { let a = multiplyTwoLists [3,2,1] [1,2,3]   == [3,4,3]
  ; let b = multiplyTwoLists [1,2,3] []        == []
  ; let c = multiplyTwoLists [5,4,3] [1,2,3,4] == [5,8,9]
  ; let d = multiplyTwoLists []      []        == []
  ; let e = multiplyTwoLists []      [1,2,3]   == []
  ; all (==(True :: Bool)) [a,b,c,d,e]
  }

-- multiply each element by a scaler
prop_MultiplyAListTest = do
  { let a = multiplyAList [3,2,1] 3 == [9,6,3]
  ; let b = multiplyAList []      1 == []
  ; let c = multiplyAList [5,4,3] 0 == [0,0,0]
  ; all (==(True :: Bool)) [a,b,c]
  }

-- divide two list together element by element to create a new list
prop_DivideTwoListTest = do
  { let a = divideTwoLists [3,2,1] [1,2,3]   == [3,1,0]
  ; let b = divideTwoLists [1,2,3] []        == []
  ; let c = divideTwoLists [5,4,3] [1,2,3,4] == [5,2,1]
  ; let d = divideTwoLists []      []        == []
  ; let e = divideTwoLists []      [1,2,3]   == []
  ; all (==(True :: Bool)) [a,b,c,d,e]
  }

-- divde each element by a scaler
prop_DivideAListTest = do
  { let a = divideAList [3,2,1] 3 == [1,0,0]
  ; let b = divideAList []      1 == []
  ; let c = divideAList [5,4,3] 0 == [5,4,3]
  ; all (==(True :: Bool)) [a,b,c]
  }

tests :: [TestTree]
tests = [ testProperty "Add List Test"      prop_AddListTest
        , testProperty "Subtract List Test" prop_SubListTest
        , testProperty "Multiply List Test" prop_MultiplyTwoListTest
        , testProperty "Scale List Test"    prop_MultiplyAListTest
        , testProperty "Divide List Test"   prop_DivideTwoListTest
        , testProperty "Reduce List Test"   prop_DivideAListTest
        ]  