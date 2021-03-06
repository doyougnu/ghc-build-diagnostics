-----------------------------------------------------------------------------
-- |
-- Module    : GHC.Packages
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Module for retrieving and updating the package list
-----------------------------------------------------------------------------


{-# OPTIONS_GHC -Wall -Werror  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module GHC.Packages
  ( retrievePackageList
  , retrieveAllPackagesBy
  , buildCache
  , unzipPackage
  , findPackageProject
  ) where


import qualified Data.Text       as T
import qualified Shelly.Lifted   as Sh

import           Control.Arrow   (second)


import           GHC.Types
import qualified GHC.Utils       as U


-- | get a list of packages from cabal
retrievePackageList :: T.Text -> ScriptM ()
retrievePackageList (T.unpack -> targetFile) =
  do ps <- Sh.silently $ T.lines <$> Sh.run "cabal" ["list", "--simple"]
     Sh.writefile targetFile (T.unlines ps)


-- | Download all packages in the package list from hackage by some predicate
retrieveAllPackagesBy :: ([(Package, Version)] -> [(Package, Version)]) -> ScriptM ()
retrieveAllPackagesBy by =
  do Sh.whenM (not <$> U.exists packageList)
       (Sh.echo "No package list in cache, rebuilding"
        >> retrievePackageList packageList)
     ps <- by . fmap (second T.tail . T.breakOn " ") . T.lines <$> Sh.readfile (Sh.fromText packageList)
     Sh.echo "Resetting tar cache"
     U.resetTarCache
     Sh.echo "Downloading packages to cache"
     U.wget ps $ T.pack (workingDir Sh.</> tarCache)


-- | Download all the most recent versions of every package from hackage
buildCache :: PackageSet -> ScriptM ()
buildCache ps =
  do Sh.echo "Checking cache"
     Sh.mkdir_p cache
     rebuilds <- U.validatePackages ps
     if not $ empty rebuilds
       then do Sh.echo $ "Rebuilding: " <> "\n" <> toText rebuilds
               Sh.cd cache
               U.cabalGetPackages rebuilds
       else Sh.echo "Everything was already cached!"


-- | uncompress a package downloaded from hackage
unzipPackage :: CompressedPackage -> ScriptM ()
unzipPackage package =
  do Sh.whenM (not <$> U.exists cache) $
       Sh.echo "Initializing Project Cache" >>
        Sh.mkdir_p cache
     U.expand package cache


findPackageProject :: Package -> ScriptM ()
findPackageProject p = Sh.cd $ workingDir Sh.</> tarCache Sh.</> p
