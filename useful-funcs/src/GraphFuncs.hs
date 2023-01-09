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
Module      : GraphFuncs
Description : A collection of value functions.
Copyright   : (c) Ancient Kraken, 2022
License     : GPL-3
Maintainer  : logical.mechanism@protonmail.com
Stability   : stable

A collection of tested on and off chain plutus graph functions
-}
module GraphFuncs
  ( colorGraph
  , createGraph
  , morphAGraph
  , computeGraphMTree
  , computeIsomorphism
  ) where
import PlutusTx.Prelude
import Plutus.V2.Ledger.Api as V2
import MathFuncs               ( baseQ )
import StringFuncs             ( byteStringAsIntegerList
                               , integerAsByteString
                               , hash
                               )
import CryptoFuncs             ( merkleTree )
-------------------------------------------------------------------------------
-- | This function colors a given graph with a minimum number of colors.
-------------------------------------------------------------------------------
{-# INLINABLE colorGraph #-}
colorGraph :: [(Integer, [Integer])] -> [(Integer, Integer)]
colorGraph graph = colorGraph' graph []
  where
    colorGraph' :: [(Integer, [Integer])] -> [(Integer, Integer)] -> [(Integer, Integer)]
    colorGraph' [] colored = colored
    colorGraph' ((node, neighbors):xs) colored =
      let c      = getMinUnusedColor neighbors colored
          colored' = (node, c) : colored
      in colorGraph' xs colored'
    
    getMinUnusedColor :: [Integer] -> [(Integer, Integer)] -> Integer
    getMinUnusedColor neighbors colored =
      let usedColors = [c | (n, c) <- colored, n `elem` neighbors]
      in findSmallestUnusedInteger usedColors

    findSmallestUnusedInteger :: [Integer] -> Integer
    findSmallestUnusedInteger ints = findSmallestUnusedInteger' [0..] ints
      where
        findSmallestUnusedInteger' :: [Integer] -> [Integer] -> Integer
        findSmallestUnusedInteger' (x:xs) ys
          | x `elem` ys = findSmallestUnusedInteger' xs ys
          | otherwise = x
-------------------------------------------------------------------------------
-- | Create a graph from a bytestring
-------------------------------------------------------------------------------
{-# INLINABLE createGraph #-}
createGraph :: V2.BuiltinByteString -> [(Integer, [Integer])]
createGraph startString = if lengthOfByteString startString /= 64 then [] else createGraph' 0 intList []
  where
    intList :: [Integer]
    intList = byteStringAsIntegerList startString

    createGraph' :: Integer -> [Integer] -> [(Integer, [Integer])] -> [(Integer, [Integer])]
    createGraph' _       []     holder = holder
    createGraph' counter (x:xs) holder = createGraph' (counter + 1) xs ((counter, baseQ (x * 123456789) 32):holder)
-------------------------------------------------------------------------------
-- | compute an isomorphic graph from a string input.
-------------------------------------------------------------------------------
computeIsomorphism :: V2.BuiltinByteString -> [Integer]
computeIsomorphism stringInput = if lengthOfByteString stringInput /= 64 then [] else createIsoMap stringInput []
  where
    createIsoMap :: V2.BuiltinByteString -> [Integer] -> [Integer]
    createIsoMap stringInput' holder =
      if length holder == 32
        then holder
        else 
          let intList = [modulo n 32 | n <- byteStringAsIntegerList stringInput' ]
              holder' = checkIfContainsAll intList holder
          in createIsoMap (hash stringInput') holder'
      where 
        checkIfContainsAll :: [Integer] -> [Integer] -> [Integer]
        checkIfContainsAll []     ys = ys
        checkIfContainsAll (x:xs) ys
          | x `elem` ys = checkIfContainsAll xs ys
          | otherwise = checkIfContainsAll xs (x:ys)
-------------------------------------------------------------------------------
-- | Morph the graph
-------------------------------------------------------------------------------
morphAGraph :: [(Integer, [Integer])] -> [Integer] -> [(Integer, [Integer])]
morphAGraph graph isomorphism = morphAGraph' graph [] isomorphism
  where
    morphAGraph' :: [(Integer, [Integer])] -> [(Integer, [Integer])] -> [Integer] -> [(Integer, [Integer])]
    morphAGraph' []                    newGraph _            = reverse newGraph
    morphAGraph' ((oldNode, edges):xs) newGraph isomorphism' = morphAGraph' xs ((isomorphism' !! oldNode, morphEdges edges [] isomorphism'):newGraph) isomorphism'

    morphEdges :: [Integer] -> [Integer] -> [Integer] -> [Integer]
    morphEdges []     holder _            = reverse holder
    morphEdges (x:xs) holder isomorphism' = morphEdges xs ((isomorphism' !! x):holder) isomorphism'
-------------------------------------------------------------------------------
-- | Computes the merkle tree of a unique graph coloring
-------------------------------------------------------------------------------
computeGraphMTree :: [(Integer, Integer)] -> V2.BuiltinByteString
computeGraphMTree coloring = merkleTree usedColorStrings
  where
    usedColorStrings :: [V2.BuiltinByteString]
    usedColorStrings = [integerAsByteString g <> integerAsByteString c | (g, c) <- coloring]