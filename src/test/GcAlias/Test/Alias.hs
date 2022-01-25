module GcAlias.Test.Alias
  ( tests
  )
  where

import qualified Data.Set as Set
import Test.Tasty
import Test.Tasty.HUnit

import GcAlias.Alias
import GcAlias.Contact


tests :: TestTree
tests = testGroup "Contacts to Aliases"
  [ contactWithOneEmail
  , contactWithTwoEmails
  ]


contactWithOneEmail :: TestTree
contactWithOneEmail = testCase "Contact with one email address" $ do
  let expected = [Alias "foo_bar" "Foo Bar" "foo@bar.com"]
  let actual = toAliases $ [ Contact (Just (Name "Foo Bar")) Nothing Nothing Set.empty
        [ ("* ", "foo@bar.com") ] ]
  expected @=? actual


contactWithTwoEmails :: TestTree
contactWithTwoEmails = testCase "Contact with two email addresses" $ do
  let expected =
        [ Alias "foo_bar_home" "Foo Bar" "foo@bar.com"
        , Alias "foo_bar_work" "Foo Bar" "foo@work.com"
        ]
  let actual = toAliases $ [ Contact (Just (Name "Foo Bar")) Nothing Nothing Set.empty
        [ ("* Home", "foo@bar.com"), ("Work", "foo@work.com") ] ]
  expected @=? actual
