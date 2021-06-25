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

module GHC.Types
  ( Package
  , Version
  , URL
  , PackageSet(..)
  , CompressedPackage(..)
  , PackageDirectory(..)
  , ProjectCache
  , ToText(..)
  , ToPath(..)
  , CabalFile(..)
  , MainFile(..)
  , Executable
  , Library
  , mkCabalFile
  , mkMainFile
  , workingDir
  , packageList
  , tarCache
  , projectCache
  , tarGz
  ) where

import qualified Data.Text as T
import qualified Shelly    as Sh


-- | Type synonyms for more descriptive types
type Package      = T.Text
type ProjectCache = T.Text
type Version      = T.Text
type URL          = T.Text


newtype CabalFile = CabalFile { unCabalFile :: T.Text }
                  deriving stock Show

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


-- | Type Class to project a type to a file path
class    ToPath a      where toPath :: a -> FilePath
instance ToPath T.Text where toPath = Sh.fromText
instance ToPath CompressedPackage where toPath = toPath . toText
instance ToPath PackageDirectory  where toPath = toPath . toText


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


projectCache :: T.Text
projectCache = "projectCache"


tarGz :: T.Text
tarGz = T.pack $ ("tar" :: T.Text) Sh.<.> "gz"
