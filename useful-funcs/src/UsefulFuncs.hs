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
Module      : UsefulFuncs
Description : A Collection of all the Useful Functions
Copyright   : (c) Ancient Kraken, 2022
License     : GPL-3
Maintainer  : logical.mechanism@protonmail.com
Stability   : stable

A collection of tested on-chain and off-chain plutus functions.
-}
module UsefulFuncs
  ( StringFuncs.integerAsByteString
  , StringFuncs.byteStringAsIntegerList
  , StringFuncs.createBuiltinByteString
  , StringFuncs.hash
  , StringFuncs.convertByteStringToInteger
  , StringFuncs.convertToString
  , AddressFuncs.isAddrGettingPaidExactly
  , AddressFuncs.isAddrHoldingToken
  , AddressFuncs.isNInputs
  , AddressFuncs.isNOutputs
  , AddressFuncs.createAddress
  , AddressFuncs.checkValidMultisig
  , TimeFuncs.lockUntilTimeInterval
  , TimeFuncs.lockBetweenTimeInterval
  , TimeFuncs.isTxOutsideInterval
  , TimeFuncs.isTxInsideInterval
  , MathFuncs.pow
  , MathFuncs.powmod
  , MathFuncs.logOfXInBaseB
  , MathFuncs.baseQ
  , MathFuncs.percentage
  , ListFuncs.replicate
  , ListFuncs.addTwoLists
  , ListFuncs.subTwoLists
  , ListFuncs.multiplyTwoLists
  , ListFuncs.multiplyAList
  , ListFuncs.divideTwoLists
  , ListFuncs.divideAList
  , ValueFuncs.checkForCurrencySymbol
  , ValueFuncs.isNRedeemers
  , ValueFuncs.adaValue
  , CryptoFuncs.verifyDiscretLogarithm
  ) where
-- useful funcs is just a wrapper around all the sub modules
import ListFuncs
import ValueFuncs
import TimeFuncs
import AddressFuncs
import StringFuncs
import MathFuncs
import CryptoFuncs