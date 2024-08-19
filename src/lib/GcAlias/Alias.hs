module GcAlias.Alias
  ( Alias (..)
  , mkAliasLine, toAliases
  )
  where

import Data.Maybe ( fromMaybe )
import qualified Data.Text as T
import Text.Printf ( printf )

import GcAlias.Common ( Email (..), Label (..), Name (..) )
import GcAlias.Contact ( Contact (..), NickName (..), Org (..) )


newtype AliasNickName = AliasNickName T.Text
  deriving (Eq, Show)

data Alias = Alias
  { alNickname :: !AliasNickName
  , alName :: !Name
  , alEmail :: !Email
  }
  deriving (Eq, Show)


toAliases :: [Contact] -> [Alias]
toAliases = concatMap oneContactToAliases


oneContactToAliases :: Contact -> [Alias]
oneContactToAliases contact = map mkAlias $ emails contact
  where
    mkAlias (label, addr) = Alias
      (mkNickname (prefix contact) label (length (emails contact) == 1))
      (fromMaybe (Name "") $ name contact) addr


mkNickname :: T.Text -> Label -> Bool -> AliasNickName
mkNickname prefix' _ True = AliasNickName . scrub $ prefix'
mkNickname prefix' (Label labelStr) False = AliasNickName . scrub $ prefix' <> " " <> labelStr


prefix :: Contact -> T.Text
prefix (Contact (Just (Name n)) _ _ _ _) = n
prefix (Contact _ (Just (NickName nn)) _ _ _) = nn
prefix (Contact _ _ (Just (Org o)) _ _) = o
prefix _ = ""


scrub :: T.Text -> T.Text
scrub = T.intercalate "_" . T.words
  . T.filter (`elem` ("abcdefghijklmnopqrstuvwxyz1234567890 " :: String))
  . T.toLower


mkAliasLine :: Alias -> String
mkAliasLine (Alias (AliasNickName nicknameStr) (Name nameStr) (Email emailStr)) =
  printf "alias %s  %s <%s>" nicknameStr nameStr emailStr
