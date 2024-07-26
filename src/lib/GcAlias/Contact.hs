{-# LANGUAGE OverloadedStrings #-}

module GcAlias.Contact
  ( Contact (..)
  , NickName (..), Org (..)
  , importAllContacts
  , importContacts
  )
  where

import Control.Arrow ( (***), second )
import Control.Newtype.Generics ( Newtype, pack )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Csv
  ( Parser, (.:), FromField, NamedRecord
  , decodeByName, FromNamedRecord, parseField, parseNamedRecord
  )
import qualified Data.HashMap.Lazy as HM
import Data.Maybe ( catMaybes, mapMaybe )
import qualified Data.Set as Set
import Data.Set ( Set )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import GHC.Generics
import Text.Printf

import GcAlias.Common ( Email (..), Label (..), Name (..) )


newtype NickName = NickName T.Text
  deriving (Generic, Show)

instance Newtype NickName

newtype Org = Org T.Text
  deriving (Generic, Show)

instance Newtype Org

newtype Group = Group T.Text
  deriving (Eq, Generic, Ord, Show)

instance Newtype Group

data Contact = Contact
  { name :: !(Maybe Name)
  , nickName :: !(Maybe NickName)
  , org :: !(Maybe Org)
  , groups :: !(Set Group)
  , emails :: ![(Label, Email)]
  }
  deriving Show

instance FromNamedRecord Contact where
  parseNamedRecord r = do
    etypes <- (map pack . catMaybes <$>) <$> mapM (r .:?) $ mkLabels "E-mail %d - Label"
    evalues <- (map pack . catMaybes <$>) <$> mapM (r .:?) $ mkLabels "E-mail %d - Value"
    let allEmails = filter (/= (pack "", pack "")) $ zip etypes evalues
    Contact
      <$> ((Name <$>)     <$> (strToMaybe <$> r .: "Name"))
      <*> ((NickName <$>) <$> (strToMaybe <$> r .: "Nickname"))
      <*> ((Org <$>)      <$> (strToMaybe <$> r .: "Organization Name"))
      <*> (splitOnColons <$> r .: "Labels")
      <*> pure allEmails


-- Our data has optional columns. Needed to implement a named record lookup
-- function that can express failure as a Maybe instead of halting parsing.
lookupMay :: FromField a => NamedRecord -> B.ByteString -> Parser (Maybe a)
lookupMay m name = maybe (pure Nothing) parseField $ HM.lookup name m


-- A convenience operator for lookupMay
(.:?) :: FromField a => NamedRecord -> B.ByteString -> Parser (Maybe a)
m .:? name = lookupMay m name


strToMaybe :: String -> Maybe T.Text
strToMaybe "" = Nothing
strToMaybe s = Just . T.pack $ s


-- The 16 labels we create here represents a larger number than we're likely to
-- see from a Google Contacts record. But it varies and will be determined by
-- the contact with the most of this particular type of field!
mkLabels :: String -> [B.ByteString]
mkLabels format = map (T.encodeUtf8 . T.pack) $ map (printf format) ([1..16] :: [Int])


splitOnColons :: String -> Set Group
splitOnColons = Set.fromList . map pack . T.splitOn " ::: " . T.pack


onlyMyContacts :: [Contact] -> [Contact]
onlyMyContacts = mapMaybe f
  where
    myContactsLabel = pack "* myContacts"
    f c@(Contact { groups = groupsSet }) =
      if Set.member myContactsLabel groupsSet
        then Just $ c { groups = Set.delete myContactsLabel groupsSet }
        else Nothing


onlyWithEmails :: [Contact] -> [Contact]
onlyWithEmails = filter $ not . null . emails


importAllContacts :: BL.ByteString -> Either String ([String], [Contact])
importAllContacts csvData = either Left
  (Right . ((V.toList . V.map (T.unpack . T.decodeUtf8)) *** V.toList))
  . decodeByName $ csvData


importContacts :: BL.ByteString -> Either String ([String], [Contact])
importContacts csvData = either Left
  (Right . second (onlyWithEmails . onlyMyContacts))
  . importAllContacts $ csvData
