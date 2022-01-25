{-# LANGUAGE OverloadedStrings #-}

module GcAlias.Contact
  ( Contact (..)
  , Name (..), NickName (..), Org (..)
  , importAllContacts
  , importContacts
  )
  where

import Control.Arrow ( (***), second )
import Control.Newtype.Generics
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Data.Csv
  ( Parser, (.:), FromField, NamedRecord
  , decodeByName, FromNamedRecord, parseField, parseNamedRecord
  )
import qualified Data.HashMap.Lazy as HM
import Data.List.Split
import Data.Maybe ( catMaybes, mapMaybe )
import qualified Data.Set as Set
import Data.Set ( Set )
import qualified Data.Vector as V
import GHC.Generics
import Text.Printf


newtype Name = Name String
  deriving (Generic, Show)

instance Newtype Name

newtype NickName = NickName String
  deriving (Generic, Show)

instance Newtype NickName

newtype Org = Org String
  deriving (Generic, Show)

instance Newtype Org

data Contact = Contact
  { name :: !(Maybe Name)
  , nickName :: !(Maybe NickName)
  , org :: !(Maybe Org)
  , groups :: !(Set String)
  , emails :: ![(String, String)]
  }
  deriving Show

instance FromNamedRecord Contact where
  parseNamedRecord r = do
    etypes <- (catMaybes <$>) <$> mapM (r .:?) $ mkLabels "E-mail %d - Type"
    evalues <- (catMaybes <$>) <$> mapM (r .:?) $ mkLabels "E-mail %d - Value"
    let allEmails = filter (/= ("","")) $ zip etypes evalues
    Contact
      <$> ((Name <$>)     <$> (strToMaybe <$> r .: "Name"))
      <*> ((NickName <$>) <$> (strToMaybe <$> r .: "Nickname"))
      <*> ((Org <$>)      <$> (strToMaybe <$> r .: "Organization 1 - Name"))
      <*> (splitOnColons <$> r .: "Group Membership")
      <*> pure allEmails


-- Our data has optional columns. Needed to implement a named record lookup
-- function that can express failure as a Maybe instead of halting parsing.
lookupMay :: FromField a => NamedRecord -> C8.ByteString -> Parser (Maybe a)
lookupMay m name = maybe (pure Nothing) parseField $ HM.lookup name m


-- An operator for lookupMay for convenience
(.:?) :: FromField a => NamedRecord -> C8.ByteString -> Parser (Maybe a)
m .:? name = lookupMay m name


strToMaybe :: String -> Maybe String
strToMaybe "" = Nothing
strToMaybe s = Just s


-- The 16 labels we create here represents a larger number than we're likely to
-- see from a Google Contacts record. But it varies and will be determined by
-- the contact with the most of this particular type of field!
mkLabels :: String -> [C8.ByteString]
mkLabels format = map C8.pack $ map (printf format) ([1..16] :: [Int])


splitOnColons :: String -> Set String
splitOnColons = Set.fromList . splitOn " ::: "


onlyMyContacts :: [Contact] -> [Contact]
onlyMyContacts = mapMaybe f
  where
    myContactsLabel = "* myContacts"
    f c@(Contact { groups = groupsSet }) =
      if Set.member myContactsLabel groupsSet
        then Just $ c { groups = Set.delete myContactsLabel groupsSet }
        else Nothing


onlyWithEmails :: [Contact] -> [Contact]
onlyWithEmails = filter $ not . null . emails


importAllContacts :: BL.ByteString -> Either String ([String], [Contact])
importAllContacts csvData = either Left
  (Right . ((V.toList . V.map C8.unpack) *** V.toList))
  . decodeByName $ csvData


importContacts :: BL.ByteString -> Either String ([String], [Contact])
importContacts csvData = either Left
  (Right . second (onlyWithEmails . onlyMyContacts))
  . importAllContacts $ csvData
