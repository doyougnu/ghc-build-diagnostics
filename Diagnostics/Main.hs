-----------------------------------------------------------------------------
-- |
-- Module    : Diagnostics
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Main module to generate diagnostics of ghc build times given an input package
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import qualified Data.Text           as T
import qualified Options.Applicative as O
import qualified Shelly              as Sh

import           Control.Applicative (some)
import           Control.Monad       (void)

import qualified GHC.Packages        as P
import qualified GHC.Diagnostics     as D
import qualified GHC.Utils           as U
import           GHC.Types

-- | The mode the command line application is run in
data Mode = Packages   PackageSet -- ^ Analyze a set of packages, read from stdin
          | BuildCache PackageSet -- ^ Download all the packages from hackage
          | Clean                 -- ^ Delete the cache


main :: IO ()
main = do
  mode <- O.execParser $ O.info (O.helper <*> parseInput) mempty
  let between before after go = do Sh.echo before
                                   void go
                                   Sh.echo after
  Sh.shelly $ case mode of
    Clean         -> between "Cleaning..." "done" (Sh.rm_rf (T.unpack workingDir))
    BuildCache ps -> U.createWorkingDir >>
                     between "Building cache" "done" (P.retrievePackages ps)
    Packages ps   -> D.diagnosePackage (head . unPackageSet $ ps)


-- | parse the input package and options
parseInput :: O.Parser Mode
parseInput = O.subparser $ packageInput <> clean <> build
  where packageInput = O.command "diagnose" $
                       O.info
                       (Packages . PackageSet <$> some (O.argument O.str (O.metavar "PACKAGE")))
                       (O.progDesc "Build and diagnose the packages passed by stdin")

        clean   = O.command "clean" $
                  O.info (pure Clean)  (O.progDesc "Clean the package cache")

        build   = O.command "cache" $
                  O.info
                  (BuildCache . PackageSet <$> some (O.argument O.str (O.metavar "PACKAGE")))
                  (O.progDesc "Cache the packages on stdin")
