{-# LANGUAGE OverloadedStrings #-}

module GcAlias.Contact
  where

-- import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Data.Csv ( (.:), decodeByName, FromNamedRecord, parseNamedRecord )
import Data.List.Split
import qualified Data.Set as Set
import Data.Set ( Set )
import qualified Data.Vector as V
import Data.Vector ( Vector )
-- import Debug.Trace
import Text.Printf


data Contact = Contact
  { name :: !String
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
      <*> (splitOnColons <$> r .: "Group Membership")
      <*> pure allEmails


mkLabels :: String -> [Int] -> [C8.ByteString]
mkLabels format = map C8.pack . map (printf format)


splitOnColons :: String -> Set String
splitOnColons = Set.fromList . splitOn " ::: "


onlyMyContacts :: Vector Contact -> Vector Contact
onlyMyContacts = V.mapMaybe f
  where
    myContactsLabel = "* myContacts"
    f c@(Contact { groups = groupsSet }) =
      if Set.member myContactsLabel groupsSet
        then Just $ c { groups = Set.delete myContactsLabel groupsSet }
        else Nothing


onlyWithEmails :: Vector Contact -> Vector Contact
onlyWithEmails = V.filter $ not . null . emails


explore :: IO ()
explore = do
  csvData <- BL.readFile "util/resources/all-contacts.csv"
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, v) -> do
      let vContacts = onlyWithEmails . onlyMyContacts $ v
      V.forM_ vContacts $ \p -> do
        print (p :: Contact)
        -- printf "\"%s\", \"%s\"\n" (name p) ()
