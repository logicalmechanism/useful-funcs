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
Description : A Collection of Useful Functions
Copyright   : (c) Ancient Kraken, 2022
License     : GPL-3
Maintainer  : logical.mechanism@protonmail.com
Stability   : stable

A collection of tested on and off chain plutus functions.
-}
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
  , pow
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
-- | Add pairs of elements together from two lists into a new list.
-- The length of the list will be the shortest input list. Empty list inputs
-- return empty list outputs.
--
-- >>> addTwoList [1,2,3] [3,2,1]
-- [4,4,4]
-- 
-- Test.Groups.List
-------------------------------------------------------------------------
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
-- Test.Groups.List
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
-- | Multiply pairs of elements together from two lists into a new list.
-- The length of the list will be the shortest input list. Empty list inputs
-- return empty list outputs.
--
-- >>> multiplyTwoList [1,2,3] [1,2,3]
-- [1,4,9]
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
-- | Multiply each element of a list by some scaler into a new list. The
-- resulting list is the same length as the input list. Empty list inputs
-- return empty list outputs.
--
-- >>> multiplyAList [1,2,3] 3
-- [3,6,9]
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
-- | Divide pairs of elements together from two lists into a new list.
-- The length of the list will be the shortest input list. Empty list inputs
-- return empty list outputs. The first list is divded by the second list.
--
-- >>> divideTwoLists [4,9,14] [1,3,7]
-- [4,3,2]
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
-- | Divide each element of a list by a scaler into a new list.
-- The length of the list will be equal to the input list. Empty list inputs
-- return empty list outputs. Dividing by zero changes nothing.
--
-- >>> divideAList [3,6,9] 3
-- [1,2,3]
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
lockBetweenTimeInterval :: Integer -> Integer -> V2.Interval V2.POSIXTime
lockBetweenTimeInterval startingTime endingTime = Interval.interval (integerToPOSIX startingTime) (integerToPOSIX endingTime)
-------------------------------------------------------------------------
-- | Create a proper time unit from an integer.
-- Not Exposed
-------------------------------------------------------------------------
integerToPOSIX :: Integer -> V2.POSIXTime
integerToPOSIX x = Time.fromMilliSeconds $ Time.DiffMilliSeconds x
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
-- | Converts a hex encoded string into a list of integers for hardcoding. Without
-- type validation there is not way to hardcode bytestring unless they are represented
-- as a list of integers that can be mapped back into a bytestring on-chain.
--
-- 
-- >>> pkh = "31ec74a9f86884e7a16f8fc30840f7f409c08b91e93d2be3a4377442"
-- >>> byteStringAsIntegerList pkh
-- [49,236,116,169,248,104,132,231,161,111,143,195,8,64,247,244,9,192,139,145,233,61,43,227,164,55,116,66]
--
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
-------------------------------------------------------------------------------
-- | Calculates x to the power of n using the exponentiation by squaring method.
--
-- >>> pow 513 3
-- 135005697
--
-- Testing: Test.Groups.Helpers
------------------------------------------------------------------------------- 
pow :: Integer -> Integer -> Integer
pow x n = 
    if n < 0 then 0 else
      if n == 0 then 1 else -- assumes 0^0=1 and not 0
      if even n 
        then pow (x * x)  (divide n 2)
        else x * pow (x * x) (divide (n - 1) 2)
-------------------------------------------------------------------------------
-- | Convert an integer into a string. This converts an integer into a list of
-- integers representing the digit in base 10. This list is feed into a mapping
-- that converts the integer into a bytestring. Great for creating incremental
-- token names.
--
-- >>> "HelloWorld_" <> integerAsByteString 42
-- "HelloWorld_42"
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
-- | Write an integer n in base q and return it as a list of integers. This
-- is the general algorithm for base conversion for any arbitrary base and number.
--
-- >>> baseQ 42 3
-- [1,1,2,0]
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
-- | Compute the sha3_256 hash of some bytestring.
--
-- >>> hash "Hello, World!"
-- "\SUB\241zfN?\168\228\EM\184\186\ENQ\194\161s\SYN\157\247ab\165\162\134\224\196\ENQ\180`\212x\247\239"
-- >>> DatumHash $ hash "Hello, World!"
-- 1af17a664e3fa8e419b8ba05c2a173169df76162a5a286e0c405b460d478f7ef
--
-- Testing: Test.Groups.Helpers
-------------------------------------------------------------------------------
hash :: V2.BuiltinByteString -> V2.BuiltinByteString
hash string = sha3_256 string
-------------------------------------------------------------------------
-- | Replicates a list l, n times. Simple replicate function.
--
-- >>> replicate [1,2,3] 2
-- [1,2,3,1,2,3]
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
-- | The log of x in base b for plutus. This is the integer division version.
--
-- >>> logOfXInBaseB 42 3
-- 3
--
-- Testing: Test.Groups.Helpers
-------------------------------------------------------------------------
logOfXInBaseB :: Integer -> Integer -> Integer
logOfXInBaseB x b = if b <= 0 then 0 else if x == b then 1 else
  if x < b
    then 0
    else 1 + logOfXInBaseB (divide x b) b
-------------------------------------------------------------------------
-- | Creates a proper BuiltinByteString type from a list of integers. This
-- is a hack solution to by pass the loss of type validation. It should be
-- used with `byteStringAsIntegerList`. This allows a bytestring to be hard
-- coded into a contract at compile time.
--
-- @
-- createBuiltinByteString $ byteStringAsIntegerList pkh
-- @
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
-- | Take in a bytestring and converts it to a number via a product. Similar
-- to creating an integer list but instead takes the product of all the values.
-- The product is limited to numbers less than 2^64 - 1.
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