{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module Groups.Address (tests) where

--------------------------------------------------------------------------------

import PlutusTx.Prelude
import Plutus.V1.Ledger.Address
import Plutus.V2.Ledger.Api        as V2
import Plutus.V1.Ledger.Value      as Value
import PlutusTx.Builtins.Internal (mkI)

--------------------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.QuickCheck

--------------------------------------------------------------------------------

import AddressFuncs ( createAddress
                    , isAddrGettingPaidExactly
                    , isAddrHoldingExactlyToken
                    , isAddrHoldingAtLeastToken
                    , isNOutputs
                    , isNInputs
                    )
import StringFuncs (hash)
--------------------------------------------------------------------------------

-- proper address check
prop_ProperAddressTest = do
  { let pkh1  = "cdace48afd957724aa22ceaf04cbc269d58b46fab2f65ed71c1cb11d"
  ; let sc1   = ""
  ; let sc2   = "c904bde2ee59b270be7d4391044eeafdf713f8c93c7534d45fd20110"
  ; let sCred = addressStakingCredential $ createAddress pkh1 sc2
  ; let a     = (addressStakingCredential $ createAddress pkh1 sc1) == Nothing
  ; let b     = retrievePkhFromStakingCredential sCred sc2
  ; all (==(True :: Bool)) [a,b]
  }

-- | Get the pkh sc from the StakingCredential type then compare.
retrievePkhFromStakingCredential :: Maybe StakingCredential -> PubKeyHash -> Bool
retrievePkhFromStakingCredential sc target =
  case sc of
    Nothing   -> False
    (Just sh) ->
      case sh of
        (StakingHash cred) ->
          case cred of
            (PubKeyCredential pkh) -> pkh == target -- compare pkhs
            _                      -> False         -- only pub key
        _                          -> False         -- only staking hash

-- test if an address can be paid by checking a tx out
prop_IsAddrGettingPaidTest = do
  { let pkh    = "cdace48afd957724aa22ceaf04cbc269d58b46fab2f65ed71c1cb11d"
  ; let sc     = ""
  ; let addr   = createAddress pkh sc
  ; let val    = Value.singleton "acab" "beef" 10
  ; let badVal = Value.singleton "acab" "beef" 5
  ; let a      = isAddrGettingPaidExactly testTxOuts addr val          -- check for token
  ; let b      = not $ isAddrGettingPaidExactly [] addr val            -- no tx outs
  ; let c      = not $ isAddrGettingPaidExactly testTxOuts addr badVal -- check for bad token
  ; all (==(True :: Bool)) [a,b,c]
  }

-- test if an address can be paid by checking a tx out
prop_IsAddrHoldingExactlyTokensTest = do
  { let pkh    = "cdace48afd957724aa22ceaf04cbc269d58b46fab2f65ed71c1cb11d"
  ; let sc     = ""
  ; let addr   = createAddress pkh sc
  ; let a      =       isAddrHoldingExactlyToken testTxOuts addr "acab" "beef" 10 -- check for token
  ; let b      = not $ isAddrHoldingExactlyToken []         addr "acab" "beef" 10 -- no tx outs
  ; let c      = not $ isAddrHoldingExactlyToken testTxOuts addr "acab" "beef" 5  -- check for bad token
  ; all (==(True :: Bool)) [a,b,c]
  }

-- test if an address can be paid by checking a tx out
prop_IsAddrHoldingAtLeastTokensTest = do
  { let pkh    = "cdace48afd957724aa22ceaf04cbc269d58b46fab2f65ed71c1cb11d"
  ; let sc     = ""
  ; let addr   = createAddress pkh sc
  ; let a      =       isAddrHoldingAtLeastToken testTxOuts addr "acab" "beef" 5 -- check for token
  ; let b      = not $ isAddrHoldingAtLeastToken []         addr "acab" "beef" 10 -- no tx outs
  ; let c      = not $ isAddrHoldingAtLeastToken testTxOuts addr "acab" "beef" 15  -- check for bad token
  ; all (==(True :: Bool)) [a,b,c]
  }

-- count how many datums are on the txOuts
prop_isNOutputsTest = do
  { let a = isNOutputs testTxOuts 2
  ;
  ; all (==(True :: Bool)) [a]
  }

-- count how many datums are on the txIns
prop_isNInputsTest = do
  { let a = isNInputs testTxInInfo 1
  ; all (==(True :: Bool)) [a]
  }

-- | a list of txOuts to use with the address testing.
testTxOuts :: [TxOut]
testTxOuts =  [goodTxOut, badTxOut]
  where
    goodTxOut = TxOut { txOutAddress         = createAddress "cdace48afd957724aa22ceaf04cbc269d58b46fab2f65ed71c1cb11d" ""
                      , txOutValue           = Value.singleton Value.adaSymbol Value.adaToken 10
                      , txOutDatum           = OutputDatumHash $ DatumHash $ hash ""
                      , txOutReferenceScript = Nothing
                      }
    
    badTxOut = TxOut { txOutAddress         = createAddress "cdace48afd957724aa22ceaf04cbc269d58b46fab2f65ed71c1cb11d" ""
                     , txOutValue           = Value.singleton "acab" "beef" 10
                     , txOutDatum           = OutputDatum $ Datum {getDatum = mkI 4}
                     , txOutReferenceScript = Nothing
                     }

testTxInInfo :: [TxInInfo]
testTxInInfo = [goodTxInInfo]
  where
    goodTxInInfo = TxInInfo { txInInfoOutRef = TxOutRef { txOutRefId = TxId $ hash ""
                                                        , txOutRefIdx = 0
                                                        }
                            , txInInfoResolved = head testTxOuts
                            }
    

tests :: [TestTree]
tests = [ testProperty "Proper Address Test"  prop_ProperAddressTest 
        , testProperty "Address Payout Test"  prop_IsAddrGettingPaidTest
        , testProperty "Holding Exactly Test" prop_IsAddrHoldingExactlyTokensTest
        , testProperty "Holding AtLeast Test" prop_IsAddrHoldingAtLeastTokensTest
        , testProperty "Is N Output Test"     prop_isNOutputsTest
        , testProperty "Is N Input Test"      prop_isNInputsTest
        ]
        