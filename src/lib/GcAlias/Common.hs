{-# LANGUAGE DeriveGeneric #-}

module GcAlias.Common
  ( ArchivePath (..)
  , CsvPath (..)
  , Email (..)
  , Name (..)
  )
  where

import Control.Newtype.Generics ( Newtype )
import GHC.Generics


newtype ArchivePath = ArchivePath FilePath
  deriving Generic

instance Newtype ArchivePath

newtype CsvPath = CsvPath FilePath
  deriving Generic

instance Newtype CsvPath

newtype Name = Name String
  deriving (Eq, Generic, Show)

instance Newtype Name

newtype Email = Email String
  deriving (Eq, Generic, Show)

instance Newtype Email
