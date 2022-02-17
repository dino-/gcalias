module GcAlias.Alias
  ( Alias (..)
  , mkAliasLine, toAliases
  )
  where

import Control.Newtype.Generics ( Newtype, op, pack )
import Data.Maybe ( fromMaybe )
import Data.Monoid
import qualified Data.Text as T
import GHC.Generics
import Text.Printf ( printf )

import GcAlias.Common ( Email (..), Label (..), Name (..) )
import GcAlias.Contact ( Contact (..), NickName (..), Org (..) )


newtype AliasNickName = AliasNickName T.Text
  deriving (Eq, Generic, Show)

instance Newtype AliasNickName

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
      (fromMaybe (pack "") $ name contact) addr


mkNickname :: T.Text -> Label -> Bool -> AliasNickName
mkNickname prefix' _ True = pack . scrub $ prefix'
mkNickname prefix' (Label labelStr) False = pack . scrub $ prefix' <> " " <> labelStr


prefix :: Contact -> T.Text
prefix contact = fromMaybe "" . getFirst . mconcat . map First $
  [ (op NickName <$> nickName contact)
  , (op Name <$> name contact)
  , (op Org <$> org contact)
  ]


scrub :: T.Text -> T.Text
scrub = T.intercalate "_" . T.words
  . T.filter (`elem` ("abcdefghijklmnopqrstuvwxyz1234567890 " :: String))
  . T.toLower


mkAliasLine :: Alias -> String
mkAliasLine (Alias (AliasNickName nicknameStr) (Name nameStr) (Email emailStr)) =
  printf "alias %s  %s <%s>" nicknameStr nameStr emailStr
