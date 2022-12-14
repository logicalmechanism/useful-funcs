{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module Groups.Value (tests) where

--------------------------------------------------------------------------------

import PlutusTx.Prelude
import Plutus.V2.Ledger.Api        as V2
import Plutus.V1.Ledger.Value      as Value
import PlutusTx.Builtins.Internal (mkI)

--------------------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.QuickCheck

--------------------------------------------------------------------------------

import ValueFuncs ( checkForCurrencySymbol
                  , isNRedeemers
                  , adaValue
                  )
--------------------------------------------------------------------------------

prop_CreateAdaSingleton = do 
  { let a = Value.isZero $ adaValue 0
  ; let b = Value.geq (adaValue 5) (adaValue 3)
  ; let c = Value.isZero ((adaValue 123) + (adaValue (-123)))
  ; all (==(True :: Bool)) [a,b,c]
  }

-- count the redeemers
prop_IsNRedeemersTest = do 
  { let a = isNRedeemers redeemers == 2
  ; all (==(True :: Bool)) [a]
  }

redeemers :: [Redeemer]
redeemers = [ Redeemer $ mkI 4
            , Redeemer $ mkI 3
            ]

-- proper address check
prop_CurrencyInListTest = do
  { let a = checkForCurrencySymbol currencies targetCurrency
  ; all (==(True :: Bool)) [a]
  }

currencies :: [CurrencySymbol]
currencies = [ CurrencySymbol ""
             , CurrencySymbol "acab"
             , CurrencySymbol "ab"
             ]

targetCurrency :: CurrencySymbol
targetCurrency = CurrencySymbol "acab"

tests :: [TestTree]
tests = [ testProperty "Currency in Currency List Test"  prop_CurrencyInListTest 
        , testProperty "Count The redeemers in the list" prop_IsNRedeemersTest
        , testProperty "Create a proper ADA singleton"   prop_CreateAdaSingleton
        ]  