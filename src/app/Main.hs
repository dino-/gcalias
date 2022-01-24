import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.IO
  ( BufferMode (NoBuffering)
  , hPutStrLn, hSetBuffering, stderr, stdout )

import GcAlias.Alias
import GcAlias.Contact
import GcAlias.TarGz


myContactsPath :: FilePath
myContactsPath = "Takeout/Contacts/My Contacts/My Contacts.csv"


main :: IO ()
main = do
  mapM_ (flip hSetBuffering $ NoBuffering) [stderr, stdout]

  paths <- parseArgs =<< getArgs
  eCsvContents <- accessFile paths
  let eContacts = importContacts =<< eCsvContents
  let eAliases = (map mkAliasLine . toAliases) <$> eContacts
  case eAliases of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right as -> mapM_ putStrLn as


parseArgs :: [String] -> IO (FilePath, FilePath)
parseArgs ("-h":_) = usage exitSuccess
parseArgs ("--help":_) = usage exitSuccess
parseArgs [archivePath, csvPath] = pure (archivePath, csvPath)
parseArgs [archivePath] = pure (archivePath, myContactsPath)
parseArgs _ = usage exitFailure


usage :: IO a -> IO a
usage exitAction = do
  hPutStrLn stderr "usage: gcalias ARCHIVE_PATH [CSV_PATH]"
  exitAction
