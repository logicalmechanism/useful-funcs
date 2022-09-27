--------------------------------------------------------------------------------
import Prelude
--------------------------------------------------------------------------------
import Test.Tasty (defaultMain, testGroup)
--------------------------------------------------------------------------------
import Groups.Helpers as Helpers
import Groups.Time    as Time
import Groups.Address as Addr
import Groups.Value   as Value
--------------------------------------------------------------------------------
main :: IO ()
main =
  defaultMain $
    testGroup
      "Groups"
      [ testGroup "Helpers" Helpers.tests
      , testGroup "Time"    Time.tests
      , testGroup "Address" Addr.tests
      , testGroup "Value"   Value.tests
      ]