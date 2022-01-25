module GcAlias.Alias
  ( Alias (..)
  , mkAliasLine, toAliases
  )
  where

import Control.Newtype.Generics ( op )
import Data.Char ( toLower )
import Data.List ( intercalate )
import Data.Maybe ( fromMaybe )
import Data.Monoid
import Text.Printf ( printf )

import GcAlias.Contact


data Alias = Alias
  { alias :: !String
  , aname :: !String
  , email :: !String
  }
  deriving (Eq, Show)


toAliases :: [Contact] -> [Alias]
toAliases = concatMap oneContactToAliases


oneContactToAliases :: Contact -> [Alias]
oneContactToAliases contact = map mkAlias $ emails contact
  where
    mkAlias (label, addr) = Alias
      (mkAliasStr (prefix contact) label (length (emails contact) == 1))
      (fromMaybe "" $ op Name <$> name contact) addr


mkAliasStr :: String -> String -> Bool -> String
mkAliasStr prefix' _ True = scrub prefix'
mkAliasStr prefix' label False = scrub (prefix' <> " " <> label)


prefix :: Contact -> String
prefix contact = fromMaybe "" . getFirst . mconcat . map First $
  [ (op NickName <$> nickName contact)
  , (op Name <$> name contact)
  , (op Org <$> org contact)
  ]


scrub :: String -> String
scrub = intercalate "_" . words
  . filter (`elem` ("abcdefghijklmnopqrstuvwxyz1234567890 " :: String))
  . map toLower


mkAliasLine :: Alias -> String
mkAliasLine (Alias alias' aname' email') =
  printf "alias %s  %s <%s>" alias' aname' email'
