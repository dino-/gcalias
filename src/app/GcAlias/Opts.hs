{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module GcAlias.Opts
  ( DumpContacts (..)
  , DumpFields (..)
  , Options (..)
  , parseOpts
  )
  where

import Control.Newtype.Generics
import Data.Version ( showVersion )
import GHC.Generics
import Options.Applicative
import Paths_gcalias ( version )
import System.Environment ( getProgName )
import Text.Heredoc ( here )
import Text.PrettyPrint.ANSI.Leijen ( string )
import Text.Printf ( printf )

import GcAlias.Common ( ArchivePath (..), CsvPath (..) )


newtype DumpContacts = DumpContacts Bool
  deriving Generic

instance Newtype DumpContacts

newtype DumpFields = DumpFields Bool
  deriving Generic

instance Newtype DumpFields

data Options = Options
  { optDumpContacts :: DumpContacts
  , optDumpFields :: DumpFields
  , optArchivePath :: ArchivePath
  , optCsvPath :: CsvPath
  }


myContactsPath :: CsvPath
myContactsPath = pack "Takeout/Contacts/My Contacts/My Contacts.csv"


parser :: Parser Options
parser = Options
  <$> ( DumpContacts <$> switch
        (  long "dump-contacts"
        <> short 'c'
        <> help "Dump of 'raw' contacts data structures"
        )
      )
  <*> ( DumpFields <$> switch
        (  long "dump-fields"
        <> short 'f'
        <> help "Dump of the record containing the field names in the CSV file"
        )
      )
  <*> ( ArchivePath <$>
        ( strArgument $ metavar "ARCHIVE_PATH" )
      )
  <*> ( maybe myContactsPath CsvPath <$> optional
        ( strArgument $ metavar "CSV_PATH" )
      )


versionHelper :: String -> Parser (a -> a)
versionHelper progName =
  infoOption (printf "%s %s" progName (showVersion version)) $ mconcat
  [ long "version"
  , help "Show version information"
  , hidden
  ]


parseOpts :: IO Options
parseOpts = do
  pn <- getProgName
  execParser $ info (parser <**> helper <**> versionHelper pn)
    (  header (printf "%s - Construct a mutt aliases file from Google Contacts CSV" pn)
    <> footer'
    )


footer' :: InfoMod a
footer' = footerDoc . Just . string . printf content . showVersion $ version
    where content = [here|OVERVIEW

gcalias is a tool for ...

Version %s  Dino Morelli <dino@ui3.info>|]
