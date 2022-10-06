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
Module      : ValueFuncs
Description : A Collection of value Functions
Copyright   : (c) Ancient Kraken, 2022
License     : GPL-3
Maintainer  : logical.mechanism@protonmail.com
Stability   : stable

A collection of tested on and off chain plutus value functions.
-}
module ValueFuncs
  ( adaValue
  , checkForCurrencySymbol
  , isNRedeemers
  ) where
import PlutusTx.Prelude
import Plutus.V2.Ledger.Api        as V2
import Plutus.V1.Ledger.Value      as Value


-- | Create a pure ada singleton with a known amount of lovelace. The amount
-- may be positive or negative depending on the use case.
--
-- >>> adaValue 123
-- Value (Map [(,Map [("",123)])])
--
-- Testing: Test.Groups.Value
-------------------------------------------------------------------------
adaValue :: Integer -> V2.Value
adaValue amt = Value.singleton Value.adaSymbol Value.adaToken amt
-------------------------------------------------------------------------
-- | Check if the policy id is in the list of policy id from some value.
-- If nothing is found or if the input is an empty list then its false.
--
-- This is designed to be combined with Value.symbols. It's a great way
-- to check if a specific policy id is contained within some validating value.
--
-- @
-- -- The list of 'CurrencySymbol's of a 'Value'.
-- symbols :: Value -> [CurrencySymbol]
-- @
-- 
-- Testing: Test.Groups.Value
-------------------------------------------------------------------------
checkForCurrencySymbol :: [V2.CurrencySymbol] -> V2.CurrencySymbol -> Bool
checkForCurrencySymbol []     _  = False
checkForCurrencySymbol (x:xs) cs =
  if x == cs
    then True 
    else checkForCurrencySymbol xs cs
-------------------------------------------------------------------------
-- | Count how many redeemers are being used inside the tx. This can be used
-- to force the number of redeemers used when spending multiple script utxos.
-- This is designed to be used with the script context inside a validation script.
--
-- @
-- toList :: Foldable t => t a -> [a]
--
-- -- A `Map` is Foldable.
-- txInfoRedeemers :: Map ScriptPurpose Redeemer
--
-- redeemers :: [Redeemer]
-- redeemers = toList $ txInfoRedeemers $ scriptContextTxInfo scriptContext
-- @
--
-- Testing: Test.Groups.Value
-------------------------------------------------------------------------
isNRedeemers :: [V2.Redeemer] -> Integer
isNRedeemers redeemers = isNRedeemers' redeemers 0
  where
    isNRedeemers' :: [V2.Redeemer] -> Integer -> Integer
    isNRedeemers' []     c = c
    isNRedeemers' (_:xs) c = isNRedeemers' xs (c + 1)