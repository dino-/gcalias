{-# LANGUAGE OverloadedStrings #-}

module GcAlias.Contact
  ( importAllContacts
  , importContacts
  )
  where

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Data.Csv ( (.:), decodeByName, FromNamedRecord, parseNamedRecord )
import Data.List.Split
import Data.Maybe ( mapMaybe )
import qualified Data.Set as Set
import Data.Set ( Set )
import qualified Data.Vector as V
import Text.Printf


data Contact = Contact
  { name :: !String
  , org :: !String
  , groups :: !(Set String)
  , emails :: ![(String, String)]
  }
  deriving Show

instance FromNamedRecord Contact where
  parseNamedRecord r = do
    etypes <- mapM (r .:) $ mkLabels "E-mail %d - Type" [1..8]
    evalues <- mapM (r .:) $ mkLabels "E-mail %d - Value" [1..8]
    let allEmails = filter (/= ("","")) $ zip etypes evalues
    Contact
      <$> r .: "Name"
      <*> r .: "Organization 1 - Name"
      <*> (splitOnColons <$> r .: "Group Membership")
      <*> pure allEmails


mkLabels :: String -> [Int] -> [C8.ByteString]
mkLabels format = map C8.pack . map (printf format)


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


importAllContacts :: FilePath -> IO (Either String [Contact])
importAllContacts filePath = do
  csvData <- BL.readFile filePath
  pure . either Left (Right . V.toList . snd) . decodeByName $ csvData


importContacts :: FilePath -> IO (Either String [Contact])
importContacts filePath = do
  allContacts <- importAllContacts filePath
  pure $ onlyWithEmails . onlyMyContacts <$> allContacts
