{-# LANGUAGE OverloadedStrings #-}

module GcAlias.Contact
  where

import Control.Arrow
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Csv as C
import Data.Csv ( (.:), decodeByName, FromNamedRecord, parseNamedRecord )
import qualified Data.Vector as V
import Text.Printf


data Contact = Contact
  { name :: !String
  , emails :: ![(String, String)]
  }
  deriving Show

-- instance FromNamedRecord Contact where
--   parseNamedRecord r = do
--     e1 <- (,) <$> r .: "E-mail 1 - Type" <*> r .: "E-mail 1 - Value"
--     e2 <- (,) <$> r .: "E-mail 2 - Type" <*> r .: "E-mail 2 - Value"
--     e3 <- (,) <$> r .: "E-mail 3 - Type" <*> r .: "E-mail 3 - Value"
--     e4 <- (,) <$> r .: "E-mail 4 - Type" <*> r .: "E-mail 4 - Value"
--     e5 <- (,) <$> r .: "E-mail 5 - Type" <*> r .: "E-mail 5 - Value"
--     e6 <- (,) <$> r .: "E-mail 6 - Type" <*> r .: "E-mail 6 - Value"
--     e7 <- (,) <$> r .: "E-mail 7 - Type" <*> r .: "E-mail 7 - Value"
--     e8 <- (,) <$> r .: "E-mail 8 - Type" <*> r .: "E-mail 8 - Value"
--     let allEmails = filter (/= ("","")) [e1, e2, e3, e4, e5, e6, e7, e8]
--     Contact
--       <$> r .: "Name"
--       <*> pure allEmails

instance FromNamedRecord Contact where
  parseNamedRecord r = do
    -- let typeLabels = mkLabelSet "E-mail %d - Type" [1..8]
    -- let valLabels = mkLabelSet "E-mail %d - Value" [1..8]
    -- :: [(String, String)]
    -- let fieldNames = zip (mkLabelSet "E-mail %d - Type" [1..8]) (mkLabelSet "E-mail %d - Value" [1..8])
    -- let etypesLabels = map (\n -> B.pack ("E-mail " <> (show n) <> " - Type")) [1..8]
    let etypesLabels = map (\n -> (("E-mail " <> (show n) <> " - Type") :: String)) [1..8]
    let evaluesLabels = map B.pack $ map (\n -> "E-mail " <> (show n) <> " - Value") [1..8]
    etypes <- mapM (r .:) etypesLabels
    evalues <- mapM (r .:) evaluesLabels
    let allEmails = filter (/= ("","")) $ zip etypes evalues
    -- :: [r -> (String, String)]
    let extractors = map (\(tl, vl) -> (.: tl) &&& (.: vl))
    -- let allEmails = filter (/= ("","")) [e1, e2, e3, e4, e5, e6, e7, e8]
    Contact
      <$> r .: "Name"
      <*> pure allEmails


explore :: IO ()
explore = do
  csvData <- BL.readFile "util/resources/all-contacts.csv"
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, v) -> V.forM_ v $ \p ->
      print (p :: Contact)
      -- printf "\"%s\", \"%s\"\n" (name p) ()
