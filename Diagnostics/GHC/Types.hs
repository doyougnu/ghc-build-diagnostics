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
  , LogFile
  , TimingsFile
  , URL
  , GhcPath(..)
  , PackageSet(..)
  , RebuildSet(..)
  , CompressedPackage(..)
  , PackageDirectory(..)
  , ProjectCache
  , ToText(..)
  , ToPath(..)
  , Empty(..)
  , CabalFile(..)
  , MainFile(..)
  , Executable
  , Library
  , mkGhcPath
  , mkCabalFile
  , mkMainFile
  , workingDir
  , packageList
  , tarCache
  , cache
  , tarGz
  , logFile
  , timingFile
  ) where

import qualified Data.Text as T
import qualified Shelly    as Sh


-- | Type synonyms for more descriptive types
type Package      = T.Text
type LogFile      = T.Text
type TimingsFile  = T.Text
type ProjectCache = FilePath
type Version      = T.Text
type URL          = T.Text

newtype GhcPath = GhcPath { unGhcPath :: T.Text }
                deriving newtype Show

newtype CabalFile = CabalFile { unCabalFile :: T.Text }
                  deriving stock Show

mkGhcPath :: Maybe T.Text -> Sh.Sh (Maybe GhcPath)
mkGhcPath Nothing    = return Nothing
mkGhcPath (Just ghc) = do version <- T.strip <$> Sh.command (toPath ghc) ["--numeric-version"] []
                          return $
                            if version == mempty
                            then Nothing
                            else Just $! GhcPath ghc

mkCabalFile :: T.Text -> Sh.Sh CabalFile
mkCabalFile = fmap (CabalFile . toText) . Sh.canonicalize . toPath

newtype MainFile a = MainFile { unMainFile :: T.Text }
                 deriving stock Show

-- | The Kind of haskell source files. Either a Main.hs file for an executable
-- package or a <package>.hs file for a library. In either case we use cabal to
-- get these and track the kind in a phantom type variable.
data Executable
data Library

mkMainFile :: T.Text -> Sh.Sh (MainFile a)
mkMainFile = fmap (MainFile . toText) . Sh.canonicalize . toPath


-- | a bunch of packages
newtype PackageSet = PackageSet { unPackageSet :: [Package] }

-- | a bunch of compilers

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


-- | Type Class to project a type to a file path
class    ToPath a      where toPath :: a -> FilePath
instance ToPath T.Text where toPath = Sh.fromText
instance ToPath CompressedPackage where toPath = toPath . toText
instance ToPath PackageDirectory  where toPath = toPath . toText

class    Empty a          where empty :: a -> Bool
instance Empty RebuildSet where empty = null . unRebuildSet

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

cache :: ProjectCache
cache = workingDir Sh.</> ("cache" :: T.Text)


tarGz :: T.Text
tarGz = T.pack $ ("tar" :: T.Text) Sh.<.> "gz"
