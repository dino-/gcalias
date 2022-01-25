import Test.Tasty

import qualified GcAlias.Test.Alias as Alias


main :: IO ()
main = defaultMain $ testGroup " tests"
  [ Alias.tests
  ]
