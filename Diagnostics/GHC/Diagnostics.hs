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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module GHC.Diagnostics where


import qualified Shelly          as Sh

import           GHC.Types
import qualified GHC.Utils       as U
import qualified GHC.Process     as P



diagnosePackage :: Package -> LogFile -> TimingsFile -> ScriptM ()
diagnosePackage p lf tf = do U.cdToPackage p
                             U.jailBreakCabal
                             U.cleanPackageDir
                             U.buildTimings
                             P.timingsToCsv p lf tf


diagnosePackageWithGhc :: Package -> LogFile -> TimingsFile -> GhcPath -> ScriptM ()
diagnosePackageWithGhc p lf tf ghc = do U.cdToPackage p
                                        U.jailBreakCabal
                                        U.cleanPackageDir
                                        U.buildTimingsWithGhc ghc
                                        P.timingsToCsv p lf tf


diagnosePackages :: PackageSet -> LogFile -> TimingsFile -> ScriptM ()
diagnosePackages (unPackageSet -> ps) lf tf =
  do rootDir <- lift Sh.pwd
     mapM_ (\p -> lift (Sh.cd rootDir) >> diagnosePackage p lf tf) ps


diagnosePackagesWithGhc :: PackageSet -> LogFile -> TimingsFile -> GhcPath -> ScriptM ()
diagnosePackagesWithGhc (unPackageSet -> ps) lf tf ghc =
  do rootDir <- lift Sh.pwd
     mapM_ (\p -> lift (Sh.cd rootDir) >> diagnosePackageWithGhc p lf tf ghc) ps
