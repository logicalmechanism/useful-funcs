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
Module      : MathFuncs
Description : A collection of math functions.
Copyright   : (c) Ancient Kraken, 2022
License     : GPL-3
Maintainer  : logical.mechanism@protonmail.com
Stability   : stable

A collection of tested on and off chain plutus math functions.
-}
module MathFuncs
  ( pow
  , powmod
  , percentage
  , baseQ
  , logOfXInBaseB
  , isIntegerInRange
  ) where
import PlutusTx.Prelude
-------------------------------------------------------------------------------
-- | Calculates x to the power of n using the exponentiation by squaring method.
--
-- >>> pow 513 3
-- 135005697
--
-- Testing: Test.Groups.Math
-------------------------------------------------------------------------------
{-# INLINABLE pow #-}
pow :: Integer -> Integer -> Integer
pow x n = 
    if n < 0 then 0 else
      if n == 0 then 1 else -- assumes 0^0=1 and not 0, sorry math
      if even n 
        then pow (x * x)  (divide n 2)
        else x * pow (x * x) (divide (n - 1) 2)
-------------------------------------------------------------------------------
-- | Calculates the x to the power of n modulo q.
--
-- >>> powmod 1425 2434 71
-- 57
--
-- Testing: Test.Groups.Math
-------------------------------------------------------------------------------
{-# INLINABLE powmod #-}
powmod :: Integer -> Integer -> Integer -> Integer
powmod x n q = 
  if q == 0 
    then 0                  -- prevent div by zero
    else modulo (pow x n) q
-------------------------------------------------------------------------------
-- | Calculate the percentage of some integer. Integer division applies here so
-- there are only so many possible outcomes. If the pct is zero then the amount
-- is zero.
--
-- >>> percentage 1234567890 40
-- 30864197
--
-- Testing: Test.Groups.Math
-------------------------------------------------------------------------------
{-# INLINABLE percentage #-}
percentage :: Integer -> Integer -> Integer
percentage amt pct = 
  if pct == 0
    then 0              -- zero percent is zero
    else divide amt pct
-------------------------------------------------------------------------------
-- | Write an integer n in base q and return it as a list of integers. This
-- is the general algorithm for base conversion for any arbitrary base and number.
--
-- >>> baseQ 42 3
-- [1,1,2,0]
--
-- Testing: Test.Groups.Math
-------------------------------------------------------------------------------
{-# INLINABLE baseQ #-}
baseQ :: Integer -> Integer -> [Integer]
baseQ number base = if base == 0 then [] else baseQ' number base []
  where
    baseQ' :: Integer -> Integer -> [Integer] -> [Integer]
    baseQ' number' base' list = do
      if number' == 0
        then list
        else baseQ' (divide number' base') base' (modulo number' base' : list)
-------------------------------------------------------------------------
-- | The log of x in base b for plutus. This is the integer division version.
--
-- >>> logOfXInBaseB 42 3
-- 3
--
-- Testing: Test.Groups.Math
-------------------------------------------------------------------------
{-# INLINABLE logOfXInBaseB #-}
logOfXInBaseB :: Integer -> Integer -> Integer
logOfXInBaseB x b = if b <= 0 then 0 else if x == b then 1 else
  if x < b
    then 0
    else 1 + logOfXInBaseB (divide x b) b
-------------------------------------------------------------------------------
-- | Check if an integer is inside a specific range determined by some slippage
-- parameter. The target value must be contained within the range of the lower
-- end and higher end. It checks if a <= x <= b for some target x and some limits
-- a and b. 
--
-- This function will return a boolean of the statement.
--
-- >>> isIntegerInRange 100 40 99
-- True
--
-- Testing: Test.Groups.Math
-------------------------------------------------------------------------------
{-# INLINABLE isIntegerInRange #-}
isIntegerInRange :: Integer -> Integer -> Integer -> Bool
isIntegerInRange value slippage target = lowEnd <= target  && -- target is equal or greater than the lower end
                                         target <= highEnd    -- target is equal or less than the higher end
  where
    lowEnd :: Integer
    lowEnd = value - percentage value slippage

    highEnd :: Integer
    highEnd = value + percentage value slippage