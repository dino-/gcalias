{-# LANGUAGE DeriveGeneric #-}

module GcAlias.Common
  ( ArchivePath (..)
  , CsvPath (..)
  , Email (..)
  , Label (..)
  , Name (..)
  )
  where

import Control.Newtype.Generics ( Newtype )
import GHC.Generics
import qualified Data.Text as T


newtype ArchivePath = ArchivePath FilePath
  deriving Generic

instance Newtype ArchivePath

newtype CsvPath = CsvPath FilePath
  deriving Generic

instance Newtype CsvPath

newtype Name = Name T.Text
  deriving (Eq, Generic, Show)

instance Newtype Name

newtype Label = Label T.Text
  deriving (Eq, Generic, Show)

instance Newtype Label

newtype Email = Email T.Text
  deriving (Eq, Generic, Show)

instance Newtype Email
