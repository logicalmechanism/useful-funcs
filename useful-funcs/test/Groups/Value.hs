{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module Groups.Value (tests) where

--------------------------------------------------------------------------------

import PlutusTx.Prelude
import Plutus.V1.Ledger.Address
import Plutus.V2.Ledger.Api        as V2
import Plutus.V2.Ledger.Contexts   as V2
import Plutus.V1.Ledger.Value      as Value
import Plutus.V1.Ledger.Time       as Time
import Plutus.V1.Ledger.Interval   as Interval
import PlutusTx.Builtins.Internal

--------------------------------------------------------------------------------

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

--------------------------------------------------------------------------------

import UsefulFuncs ( checkForCurrencySymbol
                   , isNRedeemers
                   )
--------------------------------------------------------------------------------

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
        ]  