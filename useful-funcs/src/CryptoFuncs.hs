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
  ) where
import PlutusTx.Prelude
import MathFuncs                   ( powmod )
-------------------------------------------------------------------------------
-- | Given a generator g of prime order q and agreed upon r and c constants,
-- verify that g^z = g^r * u^c mod q. This should allow a user to select some
-- secret, x, and place it into the form u = g^x for plublic viewing. A user 
-- can then pass in z = r + c*x, to prove that x is known without revealing x.
-- 
-- Testing: Test.Groups.Crypto
-------------------------------------------------------------------------------
verifyDiscretLogarithm :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Bool
verifyDiscretLogarithm g r c q z u = powmod g z q == productPowMod
  where
    productPowMod :: Integer
    productPowMod = 
      if w >= q
        then modulo w q
        else w
      where w = (powmod g r q)*(powmod u c q)