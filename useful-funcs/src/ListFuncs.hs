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
Module      : ListFuncs
Description : A Collection of List Functions
Copyright   : (c) Ancient Kraken, 2022
License     : GPL-3
Maintainer  : logical.mechanism@protonmail.com
Stability   : stable

A collection of tested on and off chain plutus list functions.
-}
module ListFuncs
  ( addTwoLists
  , subTwoLists
  , multiplyTwoLists
  , multiplyAList
  , divideTwoLists
  , divideAList
  , replicate
  ) where
import PlutusTx.Prelude
-------------------------------------------------------------------------
-- | Add pairs of elements together from two lists into a new list.
-- The length of the list will be the shortest input list. Empty list inputs
-- return empty list outputs.
--
-- >>> addTwoList [1,2,3] [3,2,1]
-- [4,4,4]
-- 
-- Testing: Test.Groups.List
-------------------------------------------------------------------------
{-# INLINABLE addTwoLists #-}
addTwoLists :: [Integer] -> [Integer] -> [Integer]
addTwoLists a b = combineLists a b []
  where
    combineLists :: [Integer] -> [Integer] -> [Integer] -> [Integer]
    combineLists _      []     out = out
    combineLists []      _     out = out
    combineLists (x:xs) (y:ys) out = combineLists xs ys (out <> [x+y])
-------------------------------------------------------------------------
-- | Subtracts pairs of elements together from two lists into a new list.
-- The length of the list will be the shortest input list. 
--
-- Only positive integers.
--
-- >>> subTwoLists [3,2,1] [1,2,3]
-- [2,0,0]
--
-- Testing: Test.Groups.List
-------------------------------------------------------------------------
{-# INLINABLE subTwoLists #-}
subTwoLists :: [Integer] -> [Integer] -> [Integer]
subTwoLists a b = combineLists a b []
  where
    combineLists :: [Integer] -> [Integer] -> [Integer] -> [Integer]
    combineLists _      []     out = out
    combineLists []      _     out = out
    combineLists (x:xs) (y:ys) out = 
      if x - y < 0 
        then combineLists xs ys (out <> [0])     -- no negatives
        else combineLists xs ys (out <> [x - y])
-------------------------------------------------------------------------
-- | Multiply pairs of elements together from two lists into a new list.
-- The length of the list will be the shortest input list. Empty list inputs
-- return empty list outputs.
--
-- >>> multiplyTwoList [1,2,3] [1,2,3]
-- [1,4,9]
--
-- Testing: Test.Groups.List
-------------------------------------------------------------------------
{-# INLINABLE multiplyTwoLists #-}
multiplyTwoLists :: [Integer] -> [Integer] -> [Integer]
multiplyTwoLists a b = combineLists a b []
  where
    combineLists :: [Integer] -> [Integer] -> [Integer] -> [Integer]
    combineLists _      []     out = out
    combineLists []      _     out = out
    combineLists (x:xs) (y:ys) out = combineLists xs ys (out <> [x * y])
-------------------------------------------------------------------------
-- | Multiply each element of a list by some scaler into a new list. The
-- resulting list is the same length as the input list. Empty list inputs
-- return empty list outputs.
--
-- >>> multiplyAList [1,2,3] 3
-- [3,6,9]
--
-- Testing: Test.Groups.List
-------------------------------------------------------------------------
{-# INLINABLE multiplyAList #-}
multiplyAList :: [Integer] -> Integer -> [Integer]
multiplyAList a scale'' = combineLists a scale'' []
  where
    combineLists :: [Integer] -> Integer -> [Integer] -> [Integer]
    combineLists []     _     out = out
    combineLists (x:xs) scale' out = combineLists xs scale' (out <> [x * scale'])
-------------------------------------------------------------------------
-- | Divide pairs of elements together from two lists into a new list.
-- The length of the list will be the shortest input list. Empty list inputs
-- return empty list outputs. The first list is divded by the second list.
--
-- >>> divideTwoLists [4,9,14] [1,3,7]
-- [4,3,2]
--
-- Testing: Test.Groups.List
-------------------------------------------------------------------------
{-# INLINABLE divideTwoLists #-}
divideTwoLists :: [Integer] -> [Integer] -> [Integer]
divideTwoLists a b = combineLists a b []
  where
    combineLists :: [Integer] -> [Integer] -> [Integer] -> [Integer]
    combineLists _      []     out = out
    combineLists []      _     out = out
    combineLists (x:xs) (y:ys) out =
      if y == 0 
        then combineLists xs ys (out <> [x])          -- keep the number
        else combineLists xs ys (out <> [divide x y]) -- integer division
-------------------------------------------------------------------------
-- | Divide each element of a list by a scaler into a new list.
-- The length of the list will be equal to the input list. Empty list inputs
-- return empty list outputs. Dividing by zero changes nothing.
--
-- >>> divideAList [3,6,9] 3
-- [1,2,3]
--
-- Testing: Test.Groups.List
-------------------------------------------------------------------------
{-# INLINABLE divideAList #-}
divideAList :: [Integer] -> Integer -> [Integer]
divideAList a scale'' = combineLists a scale'' []
  where
    combineLists :: [Integer] -> Integer -> [Integer] -> [Integer]
    combineLists []     _     out = out
    combineLists (x:xs) scale' out = 
      if scale' == 0 
        then combineLists xs scale' (out <> [x])               -- keep the number
        else combineLists xs scale' (out <> [divide x scale']) -- integer division
-------------------------------------------------------------------------
-- | Replicates a list l, n times. Simple replicate function.
--
-- >>> replicate [1,2,3] 2
-- [1,2,3,1,2,3]
--
-- Testing: Test.Groups.List
-------------------------------------------------------------------------
{-# INLINABLE replicate #-}
replicate :: [Integer] -> Integer -> [Integer]
replicate l n' = replicate' n' l []
  where
    replicate' :: Integer -> [Integer] -> [Integer] -> [Integer]
    replicate' n a o = do
      if n <= 0
        then o
        else replicate' (n-1) a (o <> a)