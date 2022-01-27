module GcAlias.Test.Alias
  ( tests
  )
  where

import Control.Newtype.Generics ( pack )
import qualified Data.Set as Set
import Test.Tasty
import Test.Tasty.HUnit

import GcAlias.Alias
import GcAlias.Common
import GcAlias.Contact


tests :: TestTree
tests = testGroup "Contacts to Aliases"
  [ contactWithOneEmail
  , contactWithTwoEmails
  ]


contactWithOneEmail :: TestTree
contactWithOneEmail = testCase "Contact with one email address" $ do
  let expected = [Alias (pack "foo_bar") (pack "Foo Bar") (pack "foo@bar.com")]
  let actual = toAliases $ [ Contact (Just (Name "Foo Bar")) Nothing Nothing Set.empty
        [ (pack "* ", pack "foo@bar.com") ] ]
  expected @=? actual


contactWithTwoEmails :: TestTree
contactWithTwoEmails = testCase "Contact with two email addresses" $ do
  let expected =
        [ Alias (pack "foo_bar_home") (pack "Foo Bar") (pack "foo@bar.com")
        , Alias (pack "foo_bar_work") (pack "Foo Bar") (pack "foo@work.com")
        ]
  let actual = toAliases $ [ Contact (Just (Name "Foo Bar")) Nothing Nothing Set.empty
        [ (pack "* Home", pack "foo@bar.com"), (pack "Work", pack "foo@work.com") ] ]
  expected @=? actual
