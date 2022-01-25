{-# LANGUAGE DeriveGeneric #-}

module GcAlias.Common
  ( ArchivePath (..)
  , CsvPath (..)
  )
  where

import Control.Newtype.Generics
import GHC.Generics


newtype ArchivePath = ArchivePath FilePath
  deriving Generic

instance Newtype ArchivePath

newtype CsvPath = CsvPath FilePath
  deriving Generic

instance Newtype CsvPath
