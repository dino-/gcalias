module GcAlias.Alias
  ( mkAliasLine, toAliases )
  where

import Control.Newtype.Generics ( op )
import Data.Char ( toLower )
import Data.List ( intercalate )
import Text.Printf ( printf )

import GcAlias.Contact


data Alias = Alias
  { alias :: !String
  , aname :: !String
  , email :: !String
  }
  deriving Show


toAliases :: [Contact] -> [Alias]
toAliases = concatMap toAlias


toAlias :: Contact -> [Alias]
toAlias contact = [Alias aliasString (maybe "" (op Name) $ name contact) (firstEmail $ emails contact)]
  where
    aliasString = mkAliasStr (name contact) (nickName contact) (org contact)

    firstEmail :: [(String, String)] -> String
    firstEmail [] = ""
    firstEmail ((_,e):_) = e


mkAliasStr :: Maybe Name -> Maybe NickName -> Maybe Org -> String
mkAliasStr _ (Just (NickName nnstr)) _ = scrub nnstr
mkAliasStr (Just (Name nstr)) _ _ = scrub nstr
mkAliasStr _ _ (Just (Org ostr)) = scrub ostr
mkAliasStr _ _ _ = "BAD!!!"  -- FIXME Need much better error handling here!!


scrub :: String -> String
scrub = intercalate "_" . words
  . filter (`elem` ("abcdefghijklmnopqrstuvwxyz1234567890 " :: String))
  . map toLower


mkAliasLine :: Alias -> String
mkAliasLine (Alias alias' aname' email') =
  printf "alias %s  %s <%s>" alias' aname' email'
