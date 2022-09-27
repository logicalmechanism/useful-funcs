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
module UsefulFuncs
  ( integerAsByteString
  , byteStringAsIntegerList
  , isAddrGettingPaidExactly
  , isAddrHoldingToken
  , isNInputs
  , isNOutputs
  , isNRedeemers
  , createAddress
  , createBuiltinByteString
  , lockUntilTimeInterval
  , lockBetweenTimeInterval
  , isTxOutsideInterval
  , isTxInsideInterval
  , hash
  , checkForCurrencySymbol
  , logOfXInBaseB
  , replicate
  , checkValidMultisig
  , baseQ
  , addTwoLists
  , subTwoLists
  , multiplyTwoLists
  , multiplyAList
  , convertByteStringToInteger
  , divideTwoLists
  , divideAList
  ) where
import PlutusTx.Prelude
import Plutus.V2.Ledger.Api        as V2
import Plutus.V2.Ledger.Contexts   as V2
import Plutus.V1.Ledger.Value      as Value
import Plutus.V1.Ledger.Time       as Time
import Plutus.V1.Ledger.Interval   as Interval
-------------------------------------------------------------------------
-- | add pairs of elements together from two lists into a new list
--
-- Testing: Test.Groups.List
-------------------------------------------------------------------------
addTwoLists :: [Integer] -> [Integer] -> [Integer]
addTwoLists a b = combineLists a b []
  where
    combineLists :: [Integer] -> [Integer] -> [Integer] -> [Integer]
    combineLists _      []     out = out
    combineLists []      _     out = out
    combineLists (x:xs) (y:ys) out = combineLists xs ys (out <> [x+y])
-------------------------------------------------------------------------
-- | subtracts pairs of elements together from two lists into a new list
--
-- Testing: Test.Groups.List
-------------------------------------------------------------------------
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
-- | multiply pairs of elements together from two lists into a new list
--
-- Testing: Test.Groups.List
-------------------------------------------------------------------------
multiplyTwoLists :: [Integer] -> [Integer] -> [Integer]
multiplyTwoLists a b = combineLists a b []
  where
    combineLists :: [Integer] -> [Integer] -> [Integer] -> [Integer]
    combineLists _      []     out = out
    combineLists []      _     out = out
    combineLists (x:xs) (y:ys) out = combineLists xs ys (out <> [x * y])
-------------------------------------------------------------------------
-- | multiply each element of a list by some scaler into a new list
--
-- Testing: Test.Groups.List
-------------------------------------------------------------------------
multiplyAList :: [Integer] -> Integer -> [Integer]
multiplyAList a scale'' = combineLists a scale'' []
  where
    combineLists :: [Integer] -> Integer -> [Integer] -> [Integer]
    combineLists []     _     out = out
    combineLists (x:xs) scale' out = combineLists xs scale' (out <> [x * scale'])
-------------------------------------------------------------------------
-- | divide pairs of elements together from two lists into a new list
--
-- Testing: Test.Groups.List
-------------------------------------------------------------------------
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
-- | divide each element of a list by some scaler into a new list
--
-- Testing: Test.Groups.List
-------------------------------------------------------------------------
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
-- | Check if the policy id is in the list of policy id from some value
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
-- | Count how many redeemers are being used inside the tx.
--
-- Testing: Test.Groups.Value
-------------------------------------------------------------------------
isNRedeemers :: [V2.Redeemer] -> Integer
isNRedeemers redeemers = isNRedeemers' redeemers 0
  where
    isNRedeemers' :: [V2.Redeemer] -> Integer -> Integer
    isNRedeemers' []     c = c
    isNRedeemers' (_:xs) c = isNRedeemers' xs (c + 1)
-------------------------------------------------------------------------
-- | Check if the validity range is inside the time interval
--
-- Testing: Test.Groups.Time
-------------------------------------------------------------------------
isTxInsideInterval :: V2.Interval V2.POSIXTime -> V2.POSIXTimeRange -> Bool
isTxInsideInterval timeRange txValidityRange = Interval.contains timeRange txValidityRange
-------------------------------------------------------------------------
-- | Check if the validity range of the tx is outside of the time interval
--
-- Testing: Test.Groups.Time
-------------------------------------------------------------------------
isTxOutsideInterval :: V2.Interval V2.POSIXTime -> V2.POSIXTimeRange -> Bool
isTxOutsideInterval timeRange txValidityRange = not $ Interval.overlaps timeRange txValidityRange
-------------------------------------------------------------------------------
-- | Pick the locking interval, assume negative inf to endingTime.
--
-- Testing: Test.Groups.Time
-------------------------------------------------------------------------------
lockUntilTimeInterval :: Integer -> V2.Interval V2.POSIXTime
lockUntilTimeInterval endingTime = Interval.to (integerToPOSIX endingTime)
-------------------------------------------------------------------------
-- | The time interval for the tx to be locked.
--
-- Testing: Test.Groups.Time
-------------------------------------------------------------------------
lockBetweenTimeInterval :: Integer -> Integer -> V2.Interval V2.POSIXTime
lockBetweenTimeInterval startingTime endingTime = Interval.interval (integerToPOSIX startingTime) (integerToPOSIX endingTime)
-------------------------------------------------------------------------
-- | Create a proper time unit from an integer
-- Not Exposed
-------------------------------------------------------------------------
integerToPOSIX :: Integer -> V2.POSIXTime
integerToPOSIX x = Time.fromMilliSeconds $ Time.DiffMilliSeconds x
-------------------------------------------------------------------------------
-- | Simple Multisig, using the info check all the pkh sigs
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
-- | Search each TxOut for an addr and value.
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
-- | Search each TxOut for an addr and value.
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
-- | Count the number of inputs that have datums of any kind.
--
-- Testing: Test.Groups.Address
-------------------------------------------------------------------------------
isNInputs :: [V2.TxInInfo] -> Integer -> Bool
isNInputs utxos number = loopInputs utxos 0
  where
    loopInputs :: [V2.TxInInfo] -> Integer  -> Bool
    loopInputs []     counter = counter == number
    loopInputs (x:xs) counter = 
      case V2.txOutDatum $ V2.txInInfoResolved x of
        V2.NoOutputDatum         -> loopInputs xs   counter
        ( V2.OutputDatumHash _ ) -> loopInputs xs ( counter + 1 ) -- embedded
        ( V2.OutputDatum     _ ) -> loopInputs xs ( counter + 1 ) -- inline
-------------------------------------------------------------------------------
-- | Count the number of outputs that have datums of any kind.
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
-- | Create a proper Address type.
--
-- Testing: Test.Groups.Address
-------------------------------------------------------------------------
createAddress :: V2.PubKeyHash -> V2.PubKeyHash -> V2.Address
createAddress pkh sc = 
  if V2.getPubKeyHash sc == emptyByteString 
    then V2.Address ( PubKeyCredential pkh ) Nothing 
    else V2.Address ( PubKeyCredential pkh ) ( Just $ StakingHash $ PubKeyCredential sc )
-------------------------------------------------------------------------------
-- | The Mapping for converting an integer into a stringed version.
-- Not Exposed
-------------------------------------------------------------------------------
integerToStringMapping :: Integer -> V2.BuiltinByteString
integerToStringMapping ch
  | ch == 0   = "0"
  | ch == 1   = "1"
  | ch == 2   = "2"
  | ch == 3   = "3"
  | ch == 4   = "4"
  | ch == 5   = "5"
  | ch == 6   = "6"
  | ch == 7   = "7"
  | ch == 8   = "8"
  | ch == 9   = "9"
  | ch == 10  = "a"
  | ch == 11  = "b"
  | ch == 12  = "c"
  | ch == 13  = "d"
  | ch == 14  = "e"
  | ch == 15  = "f"
  | otherwise = emptyByteString
-------------------------------------------------------------------------------
-- | The Mapping for converting an integer into a stringed version.
-- Not Exposed
-------------------------------------------------------------------------------
stringToIntegerMapping :: V2.BuiltinByteString -> Integer
stringToIntegerMapping ch
  | ch == "0" = 0
  | ch == "1" = 1
  | ch == "2" = 2
  | ch == "3" = 3
  | ch == "4" = 4
  | ch == "5" = 5
  | ch == "6" = 6
  | ch == "7" = 7
  | ch == "8" = 8
  | ch == "9" = 9
  | ch == "a" = 10
  | ch == "b" = 11
  | ch == "c" = 12
  | ch == "d" = 13
  | ch == "e" = 14
  | ch == "f" = 15
  | otherwise = 0
-------------------------------------------------------------------------------
-- | "Converts a hex encoded string into a list of integers for hardcoding."
--
-- Testing: Test.Groups.Helpers
-------------------------------------------------------------------------------
byteStringAsIntegerList :: V2.BuiltinByteString -> [Integer]
byteStringAsIntegerList str' = createList str' 0 []
  where
    length' :: Integer
    length' = divide (lengthOfByteString str') 2

    createList :: V2.BuiltinByteString -> Integer -> [Integer] -> [Integer]
    createList str counter value' =
      if counter >= length'
        then value'
        else createList (dropByteString 2 str) (counter+1) (value' <> [convertNumber (takeByteString 2 str) 0 1])
    
    convertNumber :: V2.BuiltinByteString -> Integer -> Integer -> Integer
    convertNumber nList value counter =
      if counter < 0
        then value
        else convertNumber (dropByteString 1 nList) (value + (pow 16 counter * val') ) (counter - 1)
      where
        first' :: V2.BuiltinByteString
        first' = takeByteString 1 nList

        val' :: Integer
        val' = stringToIntegerMapping first'
    
    pow:: Integer -> Integer -> Integer
    pow x n = if n == 0 then 1 else if n == 1 then x else
      if even n
        then pow x ( divide n 2 ) * pow x ( divide n 2 )
        else  x * pow x ( n - 1 )
-------------------------------------------------------------------------------
-- | Convert an integer into a string.
--
-- Testing: Test.Groups.Helpers
-------------------------------------------------------------------------------
integerAsByteString :: Integer -> V2.BuiltinByteString
integerAsByteString num = if num == 0 then "0" else convertToString base10 ""
  where
    base10 :: [Integer]
    base10 = baseQ num 10

    convertToString :: [Integer] -> BuiltinByteString -> BuiltinByteString
    convertToString []     str = str
    convertToString (x:xs) str = convertToString xs (str <> integerToStringMapping x)
-------------------------------------------------------------------------------
-- | Write an integer in base Q and return a list of integers.
--
-- Testing: Test.Groups.Helpers
-------------------------------------------------------------------------------
baseQ :: Integer -> Integer -> [Integer]
baseQ number base = if base == 0 then [] else baseQ' number base []
  where
    baseQ' :: Integer -> Integer -> [Integer] -> [Integer]
    baseQ' number' base' list = do
      if number' == 0
        then list
        else baseQ' (divide number' base') base' (modulo number' base' : list)
-------------------------------------------------------------------------------
-- | Compute the sha3 256 hash of some byte string
--
-- Testing: Test.Groups.Helpers
-------------------------------------------------------------------------------
hash :: V2.BuiltinByteString -> V2.BuiltinByteString
hash string = sha3_256 string
-------------------------------------------------------------------------
-- | Replicates a list, l, n times.hash 
--
-- Testing: Test.Groups.Helpers
-------------------------------------------------------------------------
replicate :: [Integer] -> Integer -> [Integer]
replicate l n' = replicate' n' l []
  where
    replicate' :: Integer -> [Integer] -> [Integer] -> [Integer]
    replicate' n a o = do
      if n <= 0
        then o
        else replicate' (n-1) a (o <> a)
-------------------------------------------------------------------------
-- | The log of x in base b in plutus
--
-- Testing: Test.Groups.Helpers
-------------------------------------------------------------------------
logOfXInBaseB :: Integer -> Integer -> Integer
logOfXInBaseB x b = if b <= 0 then 0 else if x == b then 1 else
  if x < b
    then 0
    else 1 + logOfXInBaseB (divide x b) b
-------------------------------------------------------------------------
-- | Creates a proper BuiltinByteString type.
--
-- Testing: Test.Groups.Helpers
-------------------------------------------------------------------------
createBuiltinByteString :: [Integer] -> V2.BuiltinByteString
createBuiltinByteString intList = flattenBuiltinByteString [ consByteString x emptyByteString | x <- intList]
  where
    flattenBuiltinByteString :: [V2.BuiltinByteString] -> V2.BuiltinByteString
    flattenBuiltinByteString []     = emptyByteString 
    flattenBuiltinByteString (x:xs) = appendByteString x (flattenBuiltinByteString xs)
-------------------------------------------------------------------------
-- | Take in a bytestring and convert it to a number by the product of hex number
--
-- Testing: Test.Groups.Helpers
-------------------------------------------------------------------------
convertByteStringToInteger :: BuiltinByteString -> Integer
convertByteStringToInteger hexString = 
  if hexString == emptyByteString
    then 0
    else 
      if lengthOfByteString hexString >= fixedLength + 2
        then hexStringToInteger hexString fixedLength 1 -- force length
        else 0
  where
    -- this will restrict to numbers less than 2^64 - 1
    fixedLength :: Integer
    fixedLength = 6

    -- add 1 to each number to avoid multiplying by zero
    hexStringToInteger :: BuiltinByteString -> Integer -> Integer -> Integer
    hexStringToInteger hex_string counter value'
      | counter > 0 = hexStringToInteger hex_string (counter - 1) (value' * (indexByteString hex_string counter + 1))
      | otherwise = value' * (indexByteString hex_string 0 + 1)