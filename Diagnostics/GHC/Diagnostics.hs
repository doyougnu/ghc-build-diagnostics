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
{-# LANGUAGE LambdaCase   #-}

module GHC.Diagnostics where


import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (pack)
import           System.Directory       (setCurrentDirectory)
import           System.FilePath        (takeBaseName, takeDirectory)

import qualified GHC.Packages           as P
import qualified Shelly                 as Sh

import           GHC.Types
import qualified GHC.Utils              as U


diagnosePackage :: Package -> Sh.Sh ()
diagnosePackage p =
  do current <- Sh.pwd
     -- have we seen the package before?
     U.findProject p >>= \case
       Nothing -> do Sh.echo (pack "Package not in cache...Building")
                     Sh.cd      cache
                     U.cabalGet p
       Just cached -> U.findInProject cached logFile >>= \case
         -- does the cache have a timing file?
         Nothing  -> do Sh.cd . toPath . unPackageDirectory $ cached
                        Sh.rm_rf "dist-newstyle"
                        U.buildTimings
         Just log -> Sh.echo $ pack "Log already exists for package: " <> p <> pack "...skipping"


diagnosePackages :: PackageSet -> Sh.Sh ()
diagnosePackages = mapM_ diagnosePackage . unPackageSet
