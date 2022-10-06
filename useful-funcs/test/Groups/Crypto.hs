{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module Groups.Crypto (tests) where

--------------------------------------------------------------------------------

import PlutusTx.Prelude

--------------------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.QuickCheck

--------------------------------------------------------------------------------

import CryptoFuncs ( verifyDiscretLogarithm
                   )
import MathFuncs ( pow )                   
--------------------------------------------------------------------------------

-- test if ranges are in or outside or some other range. no crossovers intervals
prop_VerifyDiscretLogarithm = do
  { let g = 3         -- generator
  ; let r = 378       -- constant r
  ; let c = 931       -- multiplier c
  ; let q = 1933      -- a prime number
  ; let x = 1232      -- a secret number only one wallet knows
  ; let y = 232       -- a bad guess at the secret number
  ; let z = r + (c*x) -- this is passed in to verify
  ; let u = pow g x   -- this is public information
  ; let h = pow g y   -- a bad guess
  ; let a = verifyDiscretLogarithm g r c q z u
  ; let b = not $ verifyDiscretLogarithm g r c q z h -- will fail
  ; all (==(True :: Bool)) [a,b]
  }


tests :: [TestTree]
tests = [ testProperty "Discret Log Test"  prop_VerifyDiscretLogarithm ]