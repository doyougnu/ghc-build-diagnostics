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

-- {-# OPTIONS_GHC -Wall -Werror  #-}

module GHC.Diagnostics where

-- Compiler
-- import           DriverPipeline
-- import           GHC
-- import           GHC.Driver.Session
-- import           GHC.CoreToStg
-- import           GHC.Paths              (libdir)
-- import           HscMain
-- import           HscTypes
-- import           GHC.Utils.Outputable
-- import           GHC.Plugins -- for ModGuts

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
-- import           CoreToStg              (coreToStg)
-- import CmmInfo            (cmmToRawCmm )
-- import CmmLint            (cmmLint)
-- import CmmPipeline        (cmmPipeline)
-- import CmmBuildInfoTables (emptySRT)
-- import AsmCodeGen         ( nativeCodeGen )
-- import UniqSupply         ( mkSplitUniqSupply, initUs_ )

-- import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           System.Directory       (setCurrentDirectory)
import           System.FilePath        (takeBaseName, takeDirectory)

import qualified GHC.Packages           as P
import qualified Shelly                 as Sh

import           GHC.Types
import qualified GHC.Utils              as U

diagnosePackage :: Package -> Sh.Sh ()
diagnosePackage p =
  do srcTarget <- toPath . unMainFile <$> packageEntryPoint p
     let mainName = takeBaseName srcTarget
         dirName  = takeDirectory srcTarget
     liftIO $ setCurrentDirectory dirName

     return ()


packageEntryPoint :: Package -> Sh.Sh (MainFile Executable)
packageEntryPoint p =
  do let pname = U.toPackageDirectory p
     P.unzipPackage $ U.toCompressedPackage p
     U.findMain pname
