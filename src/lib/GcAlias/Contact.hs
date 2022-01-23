{-# LANGUAGE OverloadedStrings #-}

module GcAlias.Contact
  where

-- import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Data.Csv ( (.:), decodeByName, FromNamedRecord, parseNamedRecord )
import qualified Data.Vector as V
-- import Debug.Trace
import Text.Printf


data Contact = Contact
  { name :: !String
  , emails :: ![(String, String)]
  }
  deriving Show

instance FromNamedRecord Contact where
  parseNamedRecord r = do
    etypes <- mapM (r .:) $ mkLabelSet "E-mail %d - Type" [1..8]
    evalues <- mapM (r .:) $ mkLabelSet "E-mail %d - Value" [1..8]
    let allEmails = filter (/= ("","")) $ zip etypes evalues
    Contact
      <$> r .: "Name"
      <*> pure allEmails


mkLabelSet :: String -> [Int] -> [C8.ByteString]
mkLabelSet format = map C8.pack . map (printf format)


explore :: IO ()
explore = do
  csvData <- BL.readFile "util/resources/all-contacts.csv"
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, v) -> V.forM_ v $ \p ->
      print (p :: Contact)
      -- printf "\"%s\", \"%s\"\n" (name p) ()
