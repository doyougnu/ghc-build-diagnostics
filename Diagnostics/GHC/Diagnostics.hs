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
import           DynFlags
import           GHC
import           GHC.Paths              (libdir)
-- import           HscMain
import           HscTypes
import           Outputable

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
import           CoreToStg              (coreToStg)
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

showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags

banner :: MonadIO m => String -> m ()
banner msg = liftIO $ putStrLn (
  replicate (fromIntegral n) '='
  ++
  msg
  ++
  replicate (fromIntegral n) '='
  )
  where
    n = (76 - length msg) `div` 2

diagnosePackage :: Package -> IO ()
diagnosePackage p =
  do srcTarget <- Sh.shelly $ toPath . unMainFile <$> packageEntryPoint p
     let mainName = takeBaseName srcTarget
         dirName  = takeDirectory srcTarget
     setCurrentDirectory dirName
     runGhc (Just libdir) $
       do env <- getSession
          dflags <- getSessionDynFlags
          setSessionDynFlags $ dflags { hscTarget = HscInterpreted }

          target <- guessTarget mainName Nothing
          setTargets [target]
          load LoadAllTargets
          modSum <- getModSummary $ mkModuleName mainName

          pmod <- parseModule modSum      -- ModuleSummary
          tmod <- typecheckModule pmod    -- TypecheckedSource
          dmod <- desugarModule tmod      -- DesugaredModule
          let core = coreModule dmod      -- CoreModule
              stg = coreToStg dflags (mg_module core) (mg_binds core)

          liftIO $ banner "Parsed Source"
          liftIO $ putStrLn $ showGhc ( parsedSource pmod )

          liftIO $ banner "Renamed Module"
          liftIO $ putStrLn $ showGhc ( tm_renamed_source tmod )

          liftIO $ banner "Typechecked Module"
          liftIO $ putStrLn $ showGhc ( tm_typechecked_source tmod )

          liftIO $ banner "Typed Toplevel Definitions"
          liftIO $ putStrLn $ showGhc ( modInfoTyThings (moduleInfo tmod) )

          liftIO $ banner "Typed Toplevel Exports"
          liftIO $ putStrLn $ showGhc ( modInfoExports (moduleInfo tmod) )


packageEntryPoint :: Package -> Sh.Sh (MainFile Executable)
packageEntryPoint p =
  do let pname = U.toPackageDirectory p
     P.unzipPackage $ U.toCompressedPackage p
     U.findMain pname
