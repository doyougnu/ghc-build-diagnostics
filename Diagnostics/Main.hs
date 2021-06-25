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

import qualified Data.Semigroup      as Semi ((<>))
import qualified Data.Text           as T
import qualified Options.Applicative as O
import qualified Shelly              as Sh

import           Control.Applicative (some, (<|>))
import           Control.Monad       (void)

import qualified GHC.Packages        as P
import qualified GHC.Diagnostics     as D
import qualified GHC.Utils           as U
import           GHC.Types

-- | The mode the command line application is run in
data Mode = Packages PackageSet -- ^ Analyze a set of packages, read from stdin
          | RefreshPList        -- ^ Refresh the package list
          | BuildCache          -- ^ Download all the packages from hackage
          | Clean               -- ^ Delete the cache


main :: IO ()
main = do
  mode <- O.execParser $ O.info (O.helper <*> parseInput) mempty
  let between before after go = do Sh.echo before
                                   void go
                                   Sh.echo after
  case mode of
    RefreshPList -> Sh.shelly $ between "Refreshing package list" "all done" $
                    P.retrievePackageList packageList
    Clean        -> Sh.shelly $
                    between "Cleaning..." "done" (Sh.rm_rf (T.unpack workingDir))
    BuildCache   -> Sh.shelly $ U.createWorkingDir >>
                    between "Building cache" "done" P.retrieveRecentPackages
    Packages ps  -> D.diagnosePackage (head . unPackageSet $ ps)


-- | parse the input package and options
parseInput :: O.Parser Mode
parseInput = packageInput <|> refresh <|> clean <|> build
  where packageInput = Packages . PackageSet <$>
                       some (O.argument O.str (O.metavar "PACKAGE"))

        refresh = O.flag' RefreshPList ( O.long "update"
                                         Semi.<> O.short 'u'
                                         Semi.<> O.help "Update the list of cabal packages"
                                       )

        clean   = O.flag' Clean ( O.long "clean"
                                  Semi.<> O.short 'c'
                                  Semi.<> O.help "clean up the working directory"
                                )

        build   = O.flag' BuildCache ( O.long "build"
                                       Semi.<> O.short 'b'
                                       Semi.<> O.help "rebuild the entire package cache"
                                     )
