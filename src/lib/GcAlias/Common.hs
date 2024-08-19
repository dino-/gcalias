module GcAlias.Common
  ( ArchivePath (..)
  , CsvPath (..)
  , Email (..)
  , Label (..)
  , Name (..)
  )
  where

import qualified Data.Text as T


newtype ArchivePath = ArchivePath FilePath

newtype CsvPath = CsvPath { v :: FilePath }

newtype Name = Name T.Text
  deriving (Eq, Show)

newtype Label = Label T.Text
  deriving (Eq, Show)

newtype Email = Email T.Text
  deriving (Eq, Show)
