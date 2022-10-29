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
Module      : CryptoFuncs
Description : A Collection of Crypto Functions
Copyright   : (c) Ancient Kraken, 2022
License     : GPL-3
Maintainer  : logical.mechanism@protonmail.com
Stability   : stable

A collection of tested on and off chain plutus crypto functions.
-}
module CryptoFuncs
  ( verifyDiscretLogarithm
  , merkleTree
  ) where
import PlutusTx.Prelude
import Plutus.V2.Ledger.Api as V2
import MathFuncs               ( powmod )
import StringFuncs             ( hash )
-------------------------------------------------------------------------------
-- | Given a generator g of prime order q and agreed upon r and c constants,
-- verify that g^z = g^r * u^c mod q. This should allow a user to select some
-- secret, x, and place it into the form u = g^x for plublic viewing. A user 
-- can then pass in z = r + c*x, to prove that x is known without revealing x.
-- 
-- Testing: Test.Groups.Crypto
-------------------------------------------------------------------------------
{-# INLINABLE verifyDiscretLogarithm #-}
verifyDiscretLogarithm :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Bool
verifyDiscretLogarithm g r c q z u = powmod g z q == productPowMod
  where
    productPowMod :: Integer
    productPowMod = 
      if w >= q
        then modulo w q
        else w
      where w = (powmod g r q)*(powmod u c q)
-------------------------------------------------------------------------------
-- | Calculate the merkle tree from a list of bytestrings.
--
-- If the list of strings is empty then the Merkle tree will return the hash of 
-- the empty bytestring. If the length is odd then the empty bytestring is appended
-- to the list such that it can be split into pairs. When the list has an even length,
-- the first step is hashing each element of the original list. Then in a pairwise fashion
-- the merkle tree is built, ultimately reutnring the hash of the final object.
--
-- Testing: Test.Groups.Crypto
-------------------------------------------------------------------------------
{-# INLINABLE merkleTree #-}
merkleTree :: [V2.BuiltinByteString] -> V2.BuiltinByteString
merkleTree listOfStrings =
  if numberOfLeaves == 0
    then hash emptyByteString                              -- an empty list hashes the empty bytestring
    else if modulo (length listOfStrings) 2 == 0
      then computeMerkleTree firstBranch []                -- calculate the merkle here
      else merkleTree (listOfStrings <> [emptyByteString]) -- append the empty bytestring to an odd length list
  where
    numberOfLeaves :: Integer
    numberOfLeaves = length listOfStrings

    -- hashes all the original bytestrings into a list of hashes.
    firstBranch :: [V2.BuiltinByteString]
    firstBranch = firstBranch' listOfStrings []
      where
        firstBranch' :: [V2.BuiltinByteString] -> [V2.BuiltinByteString] -> [V2.BuiltinByteString]
        firstBranch' []     store = store
        firstBranch' (x:xs) store = firstBranch' xs (store <> [hash x])

    -- recursively compute the tree
    computeMerkleTree :: [V2.BuiltinByteString] -> [V2.BuiltinByteString] -> V2.BuiltinByteString
    computeMerkleTree lStr storage = 
      if length lStr /= 0
        then computeMerkleTree (tail $ tail lStr) (storage <> [hash combined])
        else if numberOfBrances == 1
          then head storage                                          -- the root is returned
          else if modulo numberOfBrances 2 == 0
            then computeMerkleTree storage []                        -- continue as normal
            else computeMerkleTree (storage <> [emptyByteString]) [] -- append empty bytestring to odd length lists
      where
        numberOfBrances :: Integer
        numberOfBrances = length storage

        firstPart :: V2.BuiltinByteString
        firstPart = head lStr

        secondPart :: V2.BuiltinByteString
        secondPart = head $ tail lStr

        combined :: V2.BuiltinByteString
        combined = firstPart <> secondPart