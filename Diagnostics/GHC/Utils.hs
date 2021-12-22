-----------------------------------------------------------------------------
-- |
-- Module    : GHC.Utils
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Utilities for the diagnostic binary, mostly just wrappers around around
-- Shelly
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module GHC.Utils where

import qualified Data.Text                       as T
import qualified Shelly.Lifted                   as Sh
import Shelly.Lifted()

import           Control.Monad                   (void)
import           Data.Functor                    ((<&>))
import           Data.List                       ((\\))
import           Data.Maybe                      (isNothing)
import           System.FilePath                 (takeBaseName, takeFileName)
import           Control.Exception.Lifted (try)
import           Control.Exception        (Exception())

import qualified Distribution.PackageDescription as PD

import           GHC.Types





class    Exists a                 where exists :: a -> ScriptM Bool
instance Exists CompressedPackage where exists = exists    .
                                                 T.unpack  .
                                                 unCompressedPackage
instance Exists PackageDirectory where exists = Sh.test_d . toPath . unPackageDirectory
instance Exists FilePath         where exists = Sh.test_f
instance Exists T.Text           where exists = Sh.test_f . T.unpack


-- | Convert a package and version to a URL
toUrl :: (Package, Version) -> URL
toUrl (p,v) = hackage <> T.pack (p Sh.</> p)
              <> dash <> T.pack (v Sh.<.> tarGz)
  where
    dash    = "-" :: T.Text
    hackage = "http://hackage.haskell.org/package/"


-- | decompress a package to a package directory
expand :: CompressedPackage -> ProjectCache -> ScriptM ()
expand (unCompressedPackage -> package) (T.pack -> target) =
  do Sh.canonicalize (toPath package) >>= Sh.echo . ("Expanding: " <>) . T.pack
     Sh.canonicalize (toPath target)  >>= Sh.echo . ("To: "        <>) . T.pack
     Sh.run_ "tar" ["-xvf", package, "-C", target]
     Sh.echo "Done"


cat :: T.Text -> ScriptM T.Text
cat = Sh.run "cat" . pure


toCompressedPackage :: Package -> CompressedPackage
toCompressedPackage (T.unpack -> p) = CompressedPackage . T.pack $
                                      workingDir Sh.</> tarCache Sh.</> p Sh.<.> tarGz

toPackageDirectory :: Package -> PackageDirectory
toPackageDirectory = PackageDirectory . toText . (cache Sh.</>)


compressedToPackageDir :: CompressedPackage -> PackageDirectory
compressedToPackageDir = toPackageDirectory         .
                         T.pack                     .
                         takeBaseName               .
                         toPath                     .
                         T.dropEnd (T.length tarGz) .
                         unCompressedPackage


wget :: [(Package, Version)] -> T.Text -> ScriptM ()
wget ps target = Sh.setStdin (T.unlines . fmap toUrl $ ps) >>
                 Sh.run_ "xargs" ["wget", "--directory-prefix=" <> target]


resetTarCache :: ScriptM ()
resetTarCache = Sh.rm_rf (workingDir Sh.</> tarCache)
                >> Sh.mkdir_p (workingDir Sh.</> tarCache)


createWorkingDir :: ScriptM ()
createWorkingDir = Sh.mkdir_p (T.unpack workingDir)


createCache :: ScriptM ()
createCache = Sh.mkdir_p cache


-- | check that the cache exists, if not make it
cacheExistsOrMake :: ScriptM ()
cacheExistsOrMake = createWorkingDir >> createCache


-- | low level wrapper around find
findIn :: T.Text   -- ^ Where to look
       -> T.Text   -- ^ thing to find
       -> [T.Text] -- ^ Extra commands
       -> ScriptM T.Text
findIn there thing = go
  where go = Sh.command "find" [there, "-name", thing]


findInProject :: PackageDirectory -> T.Text -> ScriptM (Maybe T.Text)
findInProject (unPackageDirectory -> path) toFind = check <$> go
  where go = Sh.command "find" [path, "-name", toFind] []
        check b | b /= mempty = Just b
                | otherwise   = Nothing


findProject :: Package -> ScriptM (Maybe PackageDirectory)
findProject p = go <&> \case []    -> Nothing
                             (x:_) -> Just $ PackageDirectory x
  where go = T.lines <$>
          Sh.command "find"
          [toText cache, "-name", p <> "*", "-type", "d", "-maxdepth", "1"] []


isLibrary :: PD.GenericPackageDescription -> Bool
isLibrary = isNothing . PD.condLibrary


isExecutable :: PD.GenericPackageDescription -> Bool
isExecutable = null . PD.condExecutables


getMainExe :: PD.GenericPackageDescription -> [FilePath]
getMainExe = fmap (PD.modulePath . PD.condTreeData . snd) . PD.condExecutables


getDependencies' :: PD.GenericPackageDescription -> [FilePath]
getDependencies' = fmap (PD.modulePath . PD.condTreeData . snd) . PD.condExecutables


cabalGetPackages :: RebuildSet -> ScriptM ()
cabalGetPackages = mapM_ cabalGet . unRebuildSet


cabalGet :: Package -> ScriptM ()
cabalGet p = go >>= \case
  Left RedundantDir -> Sh.echo $
    "Package: " <> p <> "already in cache!, skipping the download"
  Right unit        -> return unit
  -- cabal will error if the directory already
  -- exists we catch this error here to
  -- continue processing the directory and auto
  -- succeed if the directory exists
  where
    go :: Exception e => ScriptM (Either e ())
    go = try $ Sh.command_ "cabal" ["get"] [p]

cabalBuild :: LogFile -> [T.Text] -> ScriptM ()
cabalBuild (toText -> lf) extras = void go
  where go = Sh.escaping False $ 
             Sh.command_ "cabal"
             ([ "new-build"
              , "--force-reinstalls"
              , "--allow-newer"
              , "--disable-tests"
              , "--ghc-options='-ddump-timings -v2 -fforce-recomp -O2'"
              ]
               <> extras <> [ "2>&1" -- to capture symbols sent to stderr
                            , "|"
                            , "tee"  -- track the cabal build in the log file
                            , lf
                            ]) []


cachedPackages :: ScriptM PackageSet
cachedPackages = PackageSet . fmap packageName <$> Sh.lsT cache
  where
    packageName :: Package -> Package
    packageName = fst . T.breakOn dash . T.pack . takeBaseName . T.unpack
    dash        = "-" :: T.Text

-- | remove versioning from all cabal files in current directory
jailBreakCabal :: ScriptM ()
jailBreakCabal = void
  $ Sh.escaping False
  $ Sh.command_ "jailbreak-cabal" ["*.cabal"] []

clearCabalCache :: ScriptM ()
clearCabalCache = Sh.command_ "rm" ["-rf"] ["dist-newstyle"]

clearPreviousData :: ScriptM ()
clearPreviousData = Sh.escaping False $
  Sh.command_ "rm" ["-rf"] ["*-timings.csv", "*-timings.log"]

cleanPackageDir :: ScriptM ()
cleanPackageDir = clearCabalCache >> clearPreviousData

cdToPackage :: Package -> ScriptM ()
cdToPackage p =
  do
    findProject p >>= \case
      Nothing -> do Sh.echo (T.pack "Package not in cache...Building")
                    Sh.cd cache
                    cabalGet p
                    -- find the new directory and enter it
                    findIn "." (p <> "*") [ "-type"
                                          , "d"
                                          , "-maxdepth"
                                          , "1"
                                          ] >>= Sh.cd . takeFileName . toPath . T.stripEnd
      Just cached -> Sh.cd $ toPath cached


validatePackages :: PackageSet -> ScriptM RebuildSet
validatePackages (unPackageSet -> ps) =
  do cachedPs <- unPackageSet <$> cachedPackages
     return . RebuildSet $ ps \\ cachedPs


buildTimingsBy :: [T.Text] -- ^ Extra arguments
               -> LogFile  -- ^ the log file name
               -> ScriptM ()
buildTimingsBy = flip cabalBuild


buildTimings :: ScriptM ()
buildTimings = mkLogFile >>= buildTimingsBy mempty


buildTimingsWithGhc :: GhcPath -> ScriptM ()
buildTimingsWithGhc ghc = mkLogFileWithGhc ghc >>=
                          buildTimingsBy ["-w", unGhcPath ghc]


-- | make a file by a function. Mostly just to keep the types straight
mkFileBy ::
  Separator a
  => (T.Text -> a)           -- The constructor of the file type
  -> T.Text                  -- The constant name for the file type
  -> ScriptM T.Text          -- A way to retrieve the ghc version
  -> TimeStamp               -- TimeStamp for the file
  -> ScriptM a
mkFileBy constr name getGhcVersion (toText -> time) =
  do
    version <- getGhcVersion
    return . constr $! mconcat [version, sep, time, sep, name]


-- | make a log file
mkLogFile :: ScriptM LogFile
mkLogFile = retrieveTimeStamp >>= mkFileBy LogFile logFile ghcVersion


mkTimingFile :: ScriptM TimingsFile
mkTimingFile = retrieveTimeStamp >>= mkFileBy TimingsFile timingFile ghcVersion


mkCSVFile :: ScriptM CSVFile
mkCSVFile = retrieveTimeStamp >>= mkFileBy CSVFile csvFile ghcVersion


mkLogFileWithGhc :: GhcPath -> ScriptM LogFile
mkLogFileWithGhc p = retrieveTimeStamp >>=
                     mkFileBy LogFile logFile (ghcVersionWithGhc p)


mkTimingFileWithGhc :: GhcPath -> ScriptM TimingsFile
mkTimingFileWithGhc p = retrieveTimeStamp >>=
                        mkFileBy TimingsFile timingFile (ghcVersionWithGhc p)


mkCSVFileWithGhc :: GhcPath -> ScriptM CSVFile
mkCSVFileWithGhc p = retrieveTimeStamp >>=
                     mkFileBy CSVFile csvFile (ghcVersionWithGhc p)


-- | ask the GHC on PATH what its version is
ghcVersion :: ScriptM  T.Text
ghcVersion = T.strip <$> Sh.command "ghc" ["--numeric-version"] []


-- | ask the GHC that was passed in what its version is
ghcVersionWithGhc :: GhcPath -> ScriptM T.Text
ghcVersionWithGhc (T.unpack . unGhcPath -> ghc) =
  T.strip <$> Sh.command ghc ["--numeric-version"] []


collectCSVs :: TimingsFile -> CSVFile -> ScriptM ()
collectCSVs (toText -> tf) (toText -> csv) =
  Sh.escaping False $
  Sh.command_ "find" [toText cache
                     , "-name"
                     , tf
                     , "-exec"
                     , "tail"
                     , "-n"
                     , "+2"
                     , "{}"
                     , "\\;"
                     , ">>"
                     , csv
                     ] []
