--------------------------------------------------------------------------------
import Prelude
--------------------------------------------------------------------------------
import Test.Tasty (defaultMain, testGroup)
--------------------------------------------------------------------------------
import Groups.String  as String
import Groups.Math    as Math
import Groups.Time    as Time
import Groups.Address as Addr
import Groups.Value   as Value
import Groups.List    as List
import Groups.Crypto  as Crypto
--------------------------------------------------------------------------------
main :: IO ()
main =
  defaultMain $
    testGroup
      "Groups"
      [ testGroup "String"  String.tests
      , testGroup "Math"    Math.tests
      , testGroup "Time"    Time.tests
      , testGroup "Address" Addr.tests
      , testGroup "Value"   Value.tests
      , testGroup "List"    List.tests
      , testGroup "Crypto"  Crypto.tests
      ]