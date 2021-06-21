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
{-# LANGUAGE ViewPatterns      #-}

module Main where

-- Compiler
-- import GHC
-- import DynFlags
-- import HscMain
-- import HscTypes
-- import Outputable
-- import GHC.Paths ( libdir )

-- Core Types
-- import Var
-- import Name
-- import Avail
-- import IdInfo
-- import Module
-- import Unique
-- import OccName
-- import InstEnv
-- import NameSet
-- import RdrName
-- import FamInstEnv
-- import qualified Stream
-- import qualified CoreSyn as Syn

-- Core Passes
-- import CorePrep           (corePrepPgm)
-- import CoreToStg          (coreToStg)
-- import CmmInfo            (cmmToRawCmm )
-- import CmmLint            (cmmLint)
-- import CmmPipeline        (cmmPipeline)
-- import CmmBuildInfoTables (emptySRT)
-- import AsmCodeGen         ( nativeCodeGen )
-- import UniqSupply         ( mkSplitUniqSupply, initUs_ )

import qualified Data.Text           as T
import qualified Options.Applicative as O
import qualified Shelly              as Sh
import qualified Data.Semigroup      as Semi ((<>))

import           Control.Applicative     ((<|>),some)
import           Control.Monad           (void)
import           Control.Arrow           (second)
import           Data.List.Extra         (groupOn)

import GHC.Types
import qualified GHC.Utils as U
import qualified GHC.Packages as P

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
  Sh.shelly $
    case mode of
      RefreshPList -> between "Refreshing package list" "all done" $
                      retrievePackageList packageList
      Clean        -> between "Cleaning..." "done" .
                      Sh.shelly $ Sh.rm_rf (T.unpack workingDir)
      BuildCache   -> U.createWorkingDir >>
                      between "Building cache" "done" retrieveRecentPackages
      Packages ps  -> doPackages ps


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
