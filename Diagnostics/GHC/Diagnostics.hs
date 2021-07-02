-----------------------------------------------------------------------------
-- |
-- Module    : GHC.Diagnostics
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Module for compiling a set of packages and tracking generating diagnostics
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module GHC.Diagnostics where





import qualified Shelly          as Sh

import           GHC.Types
import qualified GHC.Utils       as U
import qualified GHC.Process     as P



diagnosePackage :: Package -> LogFile -> TimingsFile -> Sh.Sh ()
diagnosePackage p lf tf = do U.cdToPackage p
                             U.buildTimings
                             P.timingsToCsv p lf tf


diagnosePackageWithGhc :: Package -> LogFile -> TimingsFile -> GhcPath -> Sh.Sh ()
diagnosePackageWithGhc p lf tf ghc = do U.cdToPackage p
                                        U.buildTimingsWithGhc ghc
                                        P.timingsToCsv p lf tf


diagnosePackages :: PackageSet -> LogFile -> TimingsFile -> Sh.Sh ()
diagnosePackages (unPackageSet -> ps) lf tf =
  do rootDir <- Sh.pwd
     mapM_ (\p -> Sh.cd rootDir >> diagnosePackage p lf tf) ps


diagnosePackagesWithGhc :: PackageSet -> LogFile -> TimingsFile -> GhcPath -> Sh.Sh ()
diagnosePackagesWithGhc (unPackageSet -> ps) lf tf ghc =
  do rootDir <- Sh.pwd
     mapM_ (\p -> Sh.cd rootDir >> diagnosePackageWithGhc p lf tf ghc) ps
