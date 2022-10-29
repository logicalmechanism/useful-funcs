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
Module      : TimeFuncs
Description : A collection of time functions.
Copyright   : (c) Ancient Kraken, 2022
License     : GPL-3
Maintainer  : logical.mechanism@protonmail.com
Stability   : stable

A collection of tested on and off chain plutus time functions.
-}
module TimeFuncs
  ( isTxInsideInterval
  , isTxOutsideInterval
  , lockUntilTimeInterval
  , lockBetweenTimeInterval
  ) where
import PlutusTx.Prelude
import Plutus.V2.Ledger.Api      as V2
import Plutus.V1.Ledger.Time     as Time
import Plutus.V1.Ledger.Interval as Interval
-------------------------------------------------------------------------
-- | Check if the validity range is inside a time interval. The validity 
-- range must be completely contained within the time range.
-- That is, @a `contains` b@ if for every entry @s@, if @member s b@ then
-- @member s a@. This is designed to be used with the script context inside 
-- a validation script.
--
-- @
-- txValidityRange :: POSIXTimeRange
-- txValidityRange = txInfoValidRange $ scriptContextTxInfo scriptContext
-- @
--
-- Testing: Test.Groups.Time
-------------------------------------------------------------------------
{-# INLINABLE isTxInsideInterval #-}
isTxInsideInterval :: V2.Interval V2.POSIXTime -> V2.POSIXTimeRange -> Bool
isTxInsideInterval timeRange txValidityRange = Interval.contains timeRange txValidityRange
-------------------------------------------------------------------------
-- | Check if the validity range of the tx is outside of the time interval,
-- that is, whether there is a value that is not a member of both intervals.
-- This does not allow for validity ranges to be on the boundary of the time range.
-- This is designed to be used with the script context inside a validation script.
--
-- @
-- txValidityRange :: POSIXTimeRange
-- txValidityRange = txInfoValidRange $ scriptContextTxInfo scriptContext
-- @
--
-- Testing: Test.Groups.Time
-------------------------------------------------------------------------
{-# INLINABLE isTxOutsideInterval #-}
isTxOutsideInterval :: V2.Interval V2.POSIXTime -> V2.POSIXTimeRange -> Bool
isTxOutsideInterval timeRange txValidityRange = not $ Interval.overlaps timeRange txValidityRange
-------------------------------------------------------------------------------
-- | Pick the locking interval, assume negative inf to endingTime. This should be
-- used to lock some utxo for all time up until some point in the future.
-- This is designed to be used with the script context inside a validation script.
--
-- >>> lockUntilTimeInterval 42
-- Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound (Finite (POSIXTime {getPOSIXTime = 42})) True}
--
-- Testing: Test.Groups.Time
-------------------------------------------------------------------------------
{-# INLINABLE lockUntilTimeInterval #-}
lockUntilTimeInterval :: Integer -> V2.Interval V2.POSIXTime
lockUntilTimeInterval endingTime = Interval.to (integerToPOSIX endingTime)
-------------------------------------------------------------------------
-- | The time interval for the tx to be locked. This is designed to create
-- a time interval between point a and point b.
--
-- >>> lockBetweenTimeInterval 17 19
-- Interval {ivFrom = LowerBound (Finite (POSIXTime {getPOSIXTime = 17})) True, ivTo = UpperBound (Finite (POSIXTime {getPOSIXTime = 19})) True}
--
-- Testing: Test.Groups.Time
-------------------------------------------------------------------------
{-# INLINABLE lockBetweenTimeInterval #-}
lockBetweenTimeInterval :: Integer -> Integer -> V2.Interval V2.POSIXTime
lockBetweenTimeInterval startingTime endingTime = Interval.interval (integerToPOSIX startingTime) (integerToPOSIX endingTime)
-------------------------------------------------------------------------
-- | Create a proper time unit from an integer.
--
-- Not Exposed
-------------------------------------------------------------------------
integerToPOSIX :: Integer -> V2.POSIXTime
integerToPOSIX x = Time.fromMilliSeconds $ Time.DiffMilliSeconds x