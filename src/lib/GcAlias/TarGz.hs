{-# LANGUAGE OverloadedStrings #-}

module GcAlias.TarGz
  ( accessFile
  )
  where

import qualified Codec.Archive.Tar as Tar
import Codec.Archive.Tar ( Entry, EntryContent (NormalFile), FormatError )
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BL


accessFile :: (FilePath, FilePath) -> IO (Either String BL.ByteString)
accessFile (tarGzPath, csvPath) = do
  tarEntries <- Tar.read . GZip.decompress <$> BL.readFile tarGzPath
  pure $ finalizeResult $ Tar.foldEntries (f csvPath) (Right []) (Left) tarEntries


f :: FilePath -> Entry -> Either e [Entry] -> Either e [Entry]
f csvPath entry (Right accumEntries)
  | Tar.entryPath entry == csvPath = Right (entry:accumEntries)
  | otherwise = Right accumEntries
f _ _ l = l


finalizeResult :: Either FormatError [Entry] -> Either String BL.ByteString
finalizeResult (Left e) = Left $ "Error: " <> show e
finalizeResult (Right (entry:_)) = case Tar.entryContent entry of
  (NormalFile bs _) -> Right bs
  _                 -> Left "Error: Unable to read the csv file"
finalizeResult _ = Left "Error: Unable to find the csv file"
