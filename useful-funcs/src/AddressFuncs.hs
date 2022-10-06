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
Module      : AddressFuncs
Description : A Collection of Address Functions
Copyright   : (c) Ancient Kraken, 2022
License     : GPL-3
Maintainer  : logical.mechanism@protonmail.com
Stability   : stable

A collection of tested on and off chain plutus address functions.
-}
module AddressFuncs
  ( checkValidMultisig
  , isAddrGettingPaidExactly
  , isAddrHoldingToken
  , createAddress
  , isNInputs -- involves implicit addr
  , isNOutputs -- involves implicit addr
  ) where
import PlutusTx.Prelude
import Plutus.V2.Ledger.Api        as V2
import Plutus.V2.Ledger.Contexts   as V2
import Plutus.V1.Ledger.Value      as Value
-------------------------------------------------------------------------------
-- | A simple multisig validation. Using the script context information, validate
-- every public key hash signature. If the public key hash is a tx signer then
-- increment the counter. At the end check if the number of signers is greater
-- than or equal to some threshold integer. This assumes the list of public key hashes
-- are known at the time of validation.
--
-- @
-- checkValidMultisig scriptContextInfo listOfPkhs threshold
-- @
--
-- Testing: Test.Groups.Address
-------------------------------------------------------------------------------
checkValidMultisig :: V2.TxInfo -> [V2.PubKeyHash] -> Integer -> Bool
checkValidMultisig txInfo pkhs thres = loopSigs pkhs 0
  where
    loopSigs :: [V2.PubKeyHash] -> Integer  -> Bool
    loopSigs []     counter = counter >= thres
    loopSigs (x:xs) counter = 
      if V2.txSignedBy txInfo x
        then loopSigs xs (counter + 1) -- just add up the good sigs
        else loopSigs xs counter       -- toss out the bad
-------------------------------------------------------------------------------
-- | Search a list of TxOut for TxOut with a specific address and value. This
-- is a simple way to check if there exist an output utxo that has exactly
-- some value and is being sent to a known address.
--
-- Testing: Test.Groups.Address
-------------------------------------------------------------------------------
isAddrGettingPaidExactly :: [V2.TxOut] -> V2.Address -> V2.Value -> Bool
isAddrGettingPaidExactly []     _    _   = False
isAddrGettingPaidExactly (x:xs) addr val
  | checkAddr && checkVal = True
  | otherwise             = isAddrGettingPaidExactly xs addr val
  where
    checkAddr :: Bool
    checkAddr = V2.txOutAddress x == addr

    checkVal :: Bool
    checkVal = V2.txOutValue x == val -- must be exact
-------------------------------------------------------------------------------
-- | Search a list of TxOut for a TxOut with a specific address that is hodling
-- an exact amount of of a singular token. This is a great function when only
-- a token is known but not the minimum amount of ada that is travelling
-- with that token during a transaction.
--
-- Testing: Test.Groups.Address
-------------------------------------------------------------------------------
isAddrHoldingToken :: [V2.TxOut] -> V2.Address -> V2.CurrencySymbol -> V2.TokenName -> Integer -> Bool
isAddrHoldingToken []     _    _   _   _   = False
isAddrHoldingToken (x:xs) addr pid tkn val
  | checkAddr && checkVal = True
  | otherwise             = isAddrHoldingToken xs addr pid tkn val 
  where
    checkAddr :: Bool
    checkAddr = V2.txOutAddress x == addr

    checkVal :: Bool
    checkVal = Value.valueOf (V2.txOutValue x) pid tkn == val -- must be exact
-------------------------------------------------------------------------------
-- | Count the number of inputs that have datums of any kind. A script input is
-- spendable if and only if it has some kind of datum, either embedded or inline.
-- Thus limiting the number of script inputs is simply limiting the number of
-- inputs that have some type of datum.
--
-- Testing: Test.Groups.Address
-------------------------------------------------------------------------------
isNInputs :: [V2.TxInInfo] -> Integer -> Bool
isNInputs utxos number = loopInputs utxos 0
  where
    loopInputs :: [V2.TxInInfo] -> Integer -> Bool
    loopInputs []     counter = counter == number
    loopInputs (x:xs) counter = 
      case V2.txOutDatum $ V2.txInInfoResolved x of
        V2.NoOutputDatum         -> loopInputs xs   counter
        ( V2.OutputDatumHash _ ) -> loopInputs xs ( counter + 1 ) -- embedded
        ( V2.OutputDatum     _ ) -> loopInputs xs ( counter + 1 ) -- inline
-------------------------------------------------------------------------------
-- | Count the number of outputs that have datums of any kind. This is great at
-- limiting the number of outputs in a transaction that can have datums. An example
-- is looking an outputs coming back to the script.
--
-- Testing: Test.Groups.Address
-------------------------------------------------------------------------------
isNOutputs :: [V2.TxOut] -> Integer -> Bool
isNOutputs utxos number = loopInputs utxos 0
  where
    loopInputs :: [V2.TxOut] -> Integer  -> Bool
    loopInputs []     counter = counter == number
    loopInputs (x:xs) counter = 
      case V2.txOutDatum x of
        V2.NoOutputDatum         -> loopInputs xs   counter
        ( V2.OutputDatumHash _ ) -> loopInputs xs ( counter + 1 ) -- embedded
        ( V2.OutputDatum     _ ) -> loopInputs xs ( counter + 1 ) -- inline
-------------------------------------------------------------------------
-- | Create a proper Address type. A transaction can input only the public
-- key hash of some wallet. This applies for payment and staking. Inside
-- plutus an address is a combination of both pkhs and is not bech32. The
-- function accounts for both types of addresses. Not to be used with 
-- validator hashes.
--
-- >>> createAddress "acab" ""
-- Address {addressCredential = PubKeyCredential acab, addressStakingCredential = Nothing}
-- >>> createAddress "acab" "beef"
-- Address {addressCredential = PubKeyCredential acab, addressStakingCredential = Just (StakingHash (PubKeyCredential beef))}
--
--
-- Testing: Test.Groups.Address
-------------------------------------------------------------------------
createAddress :: V2.PubKeyHash -> V2.PubKeyHash -> V2.Address
createAddress pkh sc = 
  if V2.getPubKeyHash sc == emptyByteString 
    then V2.Address ( PubKeyCredential pkh ) Nothing 
    else V2.Address ( PubKeyCredential pkh ) ( Just $ StakingHash $ PubKeyCredential sc )