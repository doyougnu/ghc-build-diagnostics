-----------------------------------------------------------------------------
-- |
-- Module    : GHC.Types
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Types
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror   #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHC.Types
  ( Package
  , Version
  , URL
  , LogFile(..)
  , TimingsFile(..)
  , CSVFile(..)
  , TimeStamp(..)
  , GhcPath(..)
  , PackageSet(..)
  , RebuildSet(..)
  , CompressedPackage(..)
  , PackageDirectory(..)
  , ProjectCache
  , ToText(..)
  , ToPath(..)
  , Empty(..)
  , Separator(..)
  , CabalFile(..)
  , mkGhcPath
  , workingDir
  , packageList
  , tarCache
  , cache
  , tarGz
  , logFile
  , timingFile
  , csvFile
  ) where

import qualified Data.Text as T
import qualified Shelly    as Sh

import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (UTCTime)

----------------------- Convienience Types over Text ---------------------------
-- | Type synonyms for more descriptive types
type Package      = T.Text
type ProjectCache = FilePath
type Version      = T.Text
type URL          = T.Text


-- | A wrapper for a time stamp so we don't confuse our Texts
newtype TimeStamp = TimeStamp { unTimeStamp :: UTCTime }


-- | The path of ghc
newtype GhcPath = GhcPath { unGhcPath :: T.Text }
                deriving newtype Show


-- | The path of a cabalFile
newtype CabalFile = CabalFile { unCabalFile :: T.Text }
                  deriving newtype Show


-- | A text file that is used to log the output of cabal build with timing
-- flags via tee in @cabalBuild@
newtype LogFile = LogFile { unLogFile :: T.Text }
                  deriving newtype (Show, Separator)


-- | A timing file is a csv file that is created by parsing a @LogFile@. A
-- timing file is per package and exists in a packages subdirectory in the
-- project cache, while a @CSVFile@ is the aggregation of all timing files from
-- all packages and the final output of the script. The script initializes a
-- timing file name in @main@, which is then used to write a timing file per
-- package in that package's subdirectory in the package cache.
newtype TimingsFile = TimingsFile { unTimingsFile :: T.Text }
                  deriving newtype (Show, Separator)


-- | The combination of all timing files from all packages diagnosed in the
-- package cache. This file is written to the root directory the script was
-- called from. Constructed via @collectCSVs@
newtype CSVFile = CSVFile { unCSVFile :: T.Text }
                  deriving newtype (Show, Separator)


mkGhcPath :: Maybe T.Text -> Sh.Sh (Maybe GhcPath)
mkGhcPath Nothing    = return Nothing
mkGhcPath (Just ghc) = do version <- T.strip <$> Sh.command (toPath ghc) ["--numeric-version"] []
                          return $
                            if version == mempty
                            then Nothing
                            else Just $! GhcPath ghc


-- | a bunch of packages
newtype PackageSet = PackageSet { unPackageSet :: [Package] }


-- | packages that the user is requesting but are not in the cache
newtype RebuildSet = RebuildSet { unRebuildSet :: [Package] }
                   deriving newtype (Semigroup, Monoid)


-- | The compressed package from hackage
newtype CompressedPackage = CompressedPackage { unCompressedPackage :: Package }
                          deriving stock Show


-- | The decompressed project directory for a given package from hackage. These
-- should always occur in the ProjectCache.
newtype PackageDirectory = PackageDirectory  { unPackageDirectory :: Package }
                         deriving stock Show


-- | Type Class to project a type to text
class    ToText a where toText :: a -> T.Text
instance ToText CompressedPackage where toText = unCompressedPackage
instance ToText PackageDirectory  where toText = unPackageDirectory
instance ToText FilePath          where toText = Sh.toTextIgnore
instance ToText RebuildSet        where toText = T.unlines . unRebuildSet
instance ToText PackageSet        where toText = T.unlines . unPackageSet
instance ToText LogFile           where toText = unLogFile
instance ToText TimingsFile       where toText = unTimingsFile
instance ToText CSVFile           where toText = unCSVFile
instance ToText TimeStamp         where toText = T.pack . formatTime defaultTimeLocale "%F-%T" . unTimeStamp
instance Show a => ToText (Maybe a) where toText = toText . show


-- | Type Class to project a type to a file path
class    ToPath a      where toPath :: a -> FilePath
instance ToPath T.Text where toPath = Sh.fromText
instance ToPath CompressedPackage where toPath = toPath . toText
instance ToPath PackageDirectory  where toPath = toPath . toText
instance ToPath LogFile           where toPath = toPath . toText
instance ToPath TimingsFile       where toPath = toPath . toText


-- |
class    Empty a          where empty :: a -> Bool
instance Empty RebuildSet where empty = null . unRebuildSet


class    Separator a      where sep :: a
instance Separator T.Text where sep = "-"

-- | constant for directory to hold packages
workingDir :: T.Text
workingDir = ".ghc-diagnostics"


-- | constant for the file to hold a list of packages
packageList :: T.Text
packageList = T.pack (workingDir Sh.</> p)
  where p :: T.Text
        p = "package-list"


tarCache :: T.Text
tarCache = "tarCache"


logFile :: T.Text
logFile = "timings.log"


timingFile :: T.Text
timingFile = "timings.csv"


csvFile :: T.Text
csvFile = "data.csv"


cache :: ProjectCache
cache = workingDir Sh.</> ("cache" :: T.Text)


tarGz :: T.Text
tarGz = T.pack $ ("tar" :: T.Text) Sh.<.> "gz"
