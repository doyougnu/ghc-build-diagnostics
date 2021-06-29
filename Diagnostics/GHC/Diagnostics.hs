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



import           Data.Text       (pack,stripEnd)
import           System.FilePath (takeFileName)

import qualified Shelly          as Sh

import           GHC.Types
import qualified GHC.Utils       as U


diagnosePackage :: Package -> Sh.Sh ()
diagnosePackage p =
  do -- have we seen the package before?
     U.findProject p >>= \case
       Nothing -> do Sh.echo (pack "Package not in cache...Building")
                     Sh.cd cache
                     U.cabalGet p
                     -- find the new directory and enter it
                     U.findIn "." (p <> "*") [ "-type"
                                             , "d"
                                             , "-maxdepth"
                                             , "1"
                                             ] >>= Sh.cd . takeFileName . toPath . stripEnd
                     U.buildTimings
       Just cached -> U.findInProject cached logFile >>= \case
         -- does the cache have a timing file?
         Nothing  -> do Sh.cd . toPath . unPackageDirectory $ cached
                        Sh.rm_rf "dist-newstyle"
                        U.buildTimings
         Just _   -> Sh.echo $ pack "Log already exists for package: " <> p <> pack "...skipping"


diagnosePackages :: PackageSet -> Sh.Sh ()
diagnosePackages (unPackageSet -> ps) =
  do rootDir <- Sh.pwd
     mapM_ (\p -> Sh.cd rootDir >> diagnosePackage p) ps
