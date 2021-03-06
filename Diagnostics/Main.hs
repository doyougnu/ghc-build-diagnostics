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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import qualified Data.Text           as T
import qualified Options.Applicative as O
import qualified Shelly.Lifted       as Sh

import           Control.Applicative (some)
import           Control.Monad       (void)

import qualified GHC.Packages        as P

import qualified GHC.Diagnostics     as D
import qualified GHC.Utils           as U
import           GHC.Types

-- | The mode the command line application is run in
data Mode = Packages   PackageSet (Maybe T.Text) -- ^ Analyze a set of packages, read from stdin
          | BuildCache PackageSet                -- ^ Download all the packages from hackage
          | Clean                                -- ^ Delete the cache


main :: IO ()
main = do
  mode <- O.execParser $ O.info (O.helper <*> parseInput) mempty
  let between before after go = do Sh.echo before
                                   void go
                                   Sh.echo after
  env <- initEnv
  runScript env $ case mode of
    Clean          -> between "Cleaning..." "done" (Sh.rm_rf (T.unpack workingDir))
    BuildCache ps  -> U.cacheExistsOrMake >>
                      between "Building cache" "done" (P.buildCache ps)
    Packages ps ghcPath' -> do Sh.echo $ "Diagnosing: \n" <> toText ps <> "\n" <> toText ghcPath'
                               _    <- U.cacheExistsOrMake
                               root <- Sh.pwd
                               mkGhcPath ghcPath' >>= \case
                                 Nothing      -> do tf   <- U.mkTimingFile
                                                    lf   <- U.mkLogFile
                                                    csv  <- U.mkCSVFile
                                                    D.diagnosePackages ps lf tf
                                                    Sh.cd root >> U.collectCSVs tf csv
                                 Just ghcPath -> do tf'   <- U.mkTimingFileWithGhc ghcPath
                                                    lf'   <- U.mkLogFileWithGhc ghcPath
                                                    csv'  <- U.mkCSVFileWithGhc ghcPath
                                                    D.diagnosePackagesWithGhc ps lf' tf' ghcPath
                                                    Sh.cd root >> U.collectCSVs tf' csv'



-- | parse the input package and options
parseInput :: O.Parser Mode
parseInput = O.subparser $ packageInput <> clean <> build
  where packageInput = O.command "diagnose" $
                       O.info
                       (Packages
                        <$> (PackageSet <$> some (O.argument O.str (O.metavar "PACKAGE")))
                        <*> O.optional (O.strOption ( O.long "ghc"
                                         <> O.short 'g'
                                         <> O.metavar "GHCSFILE"
                                         <> O.help "path to a ghc version to use")))

                       (O.progDesc "Build and diagnose the packages passed by stdin")

        clean   = O.command "clean" $
                  O.info (pure Clean)  (O.progDesc "Clean the package cache")

        build   = O.command "cache" $
                  O.info
                  (BuildCache . PackageSet <$> some (O.argument O.str (O.metavar "PACKAGE")))
                  (O.progDesc "Cache the packages on stdin")
