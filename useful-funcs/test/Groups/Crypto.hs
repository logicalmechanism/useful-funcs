{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module Groups.Crypto (tests) where

--------------------------------------------------------------------------------

import PlutusTx.Prelude
import Plutus.V2.Ledger.Api as V2

--------------------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.QuickCheck

--------------------------------------------------------------------------------
import CryptoFuncs ( verifyDiscretLogarithm
                   , merkleTree
                   )
import MathFuncs   ( pow )
import StringFuncs ( hash 
                   , integerAsByteString
                   )
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

-- test the creation of a merkle tree
prop_MerkleTreeTest = do
  { let a = hash "" == merkleTree []
  ; let b = hash ( (hash "")  <> (hash "") ) == merkleTree ["", ""]
  ; let c = hash ( (hash "")  <> (hash "") ) == merkleTree [""]
  ; let d = hash ( (hash "a") <> (hash "b")) == merkleTree ["a", "b"]
  ; let e = hash ( (hash "a") <> (hash "") ) == merkleTree ["a"]
  ; let f = (DatumHash $ merkleTree [integerAsByteString f' | f' <- [1..500]]) == "a38aecc7812b0df0552901e76f5440190f26fbb9d0ef4f977e60a100fb6f33e3"
  ; let g = merkleTree [integerAsByteString f' | f' <- [1..300]] /= merkleTree [integerAsByteString f' | f' <- [1..400]]
  ; all (==(True :: Bool)) [a,b,c,d,e,f,g]
  }

tests :: [TestTree]
tests = [ testProperty "Discret Log Test"  prop_VerifyDiscretLogarithm
        , testProperty "Merkle Tree Test" prop_MerkleTreeTest
        ]