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
import qualified Shelly                          as Sh

import           Control.Exception.Base          (SomeException)
import           Control.Monad                   (void)
import           Data.Functor                    ((<&>))
import           Data.List                       ((\\))
import           Data.Maybe                      (isNothing)
import           System.FilePath                 (takeBaseName, takeFileName)

import qualified Distribution.PackageDescription as PD


import           GHC.Types


class    Exists a                 where exists :: a -> Sh.Sh Bool
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
expand :: CompressedPackage -> ProjectCache -> Sh.Sh ()
expand (unCompressedPackage -> package) (T.pack -> target) =
  do Sh.canonicalize (toPath package) >>= Sh.echo . ("Expanding: " <>) . T.pack
     Sh.canonicalize (toPath target)  >>= Sh.echo . ("To: "        <>) . T.pack
     Sh.run_ "tar" ["-xvf", package, "-C", target]
     Sh.echo "Done"


cat :: T.Text -> Sh.Sh T.Text
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


wget :: [(Package, Version)] -> T.Text -> Sh.Sh ()
wget ps target = Sh.setStdin (T.unlines . fmap toUrl $ ps) >>
                 Sh.run_ "xargs" ["wget", "--directory-prefix=" <> target]


resetTarCache :: Sh.Sh ()
resetTarCache = Sh.rm_rf (workingDir Sh.</> tarCache)
                >> Sh.mkdir_p (workingDir Sh.</> tarCache)


createWorkingDir :: Sh.Sh ()
createWorkingDir = Sh.mkdir_p (T.unpack workingDir)


createCache :: Sh.Sh ()
createCache = Sh.mkdir_p cache


-- | check that the cache exists, if not make it
cacheExistsOrMake :: Sh.Sh ()
cacheExistsOrMake = createWorkingDir >> createCache


-- | low level wrapper around find
findIn :: T.Text   -- ^ Where to look
       -> T.Text   -- ^ thing to find
       -> [T.Text] -- ^ Extra commands
       -> Sh.Sh T.Text
findIn there thing = go
  where go = Sh.command "find" [there, "-name", thing]


findInProject :: PackageDirectory -> T.Text -> Sh.Sh (Maybe T.Text)
findInProject (unPackageDirectory -> path) toFind = check <$> go
  where go = Sh.command "find" [path, "-name", toFind] []
        check b | b /= mempty = Just b
                | otherwise   = Nothing


findProject :: Package -> Sh.Sh (Maybe PackageDirectory)
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


cabalGetPackages :: RebuildSet -> Sh.Sh ()
cabalGetPackages = mapM_ cabalGet . unRebuildSet


cabalGet :: Package -> Sh.Sh ()
cabalGet p = void $! trySh go
  -- cabal will error if the directory already
  -- exists we catch this error here to
  -- continue processing the directory and auto
  -- succeed if the directory exists
  where
    go :: Sh.Sh ()
    go = Sh.command_ "cabal" ["get"] [p]

trySh :: Sh.Sh a -> Sh.Sh (Either SomeException a)
trySh a = Sh.catch_sh (Right <$> a) (return . Left)

cabalBuild :: LogFile -> [T.Text] -> Sh.Sh ()
cabalBuild (toText -> lf) extras = void go
  where go = Sh.escaping False $ trySh $
             Sh.command_ "cabal"
             ([ "new-build"
              , "--force-reinstalls"
              , "--allow-newer"
              , "--disable-tests"
              , "--ghc-options='-ddump-timings -v2 -fforce-recomp -O2'"
              ]
               <> extras <> [ "2>&1" -- to capture symbols sent to stderr
                            , "|"
                            , "tee"
                            , lf
                            ]) []


cachedPackages :: Sh.Sh PackageSet
cachedPackages = PackageSet . fmap packageName <$> Sh.lsT cache
  where
    packageName :: Package -> Package
    packageName = fst . T.breakOn dash . T.pack . takeBaseName . T.unpack
    dash        = "-" :: T.Text

-- | remove versioning from all cabal files in current directory
jailBreakCabal :: Sh.Sh ()
jailBreakCabal = void
  $ Sh.escaping False
  $ trySh
  $ Sh.command_ "jailbreak-cabal" ["*.cabal"] []

clearCabalCache :: Sh.Sh ()
clearCabalCache = Sh.command_ "rm" ["-rf"] ["dist-newstyle"]

cdToPackage :: Package -> Sh.Sh ()
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


validatePackages :: PackageSet -> Sh.Sh RebuildSet
validatePackages (unPackageSet -> ps) =
  do cachedPs <- unPackageSet <$> cachedPackages
     return . RebuildSet $ ps \\ cachedPs


buildTimingsBy :: [T.Text] -- ^ Extra arguments
               -> LogFile  -- ^ the log file name
               -> Sh.Sh ()
buildTimingsBy = flip cabalBuild


buildTimings :: Sh.Sh ()
buildTimings = mkLogFileBy ghcVersion >>= buildTimingsBy mempty


buildTimingsWithGhc :: GhcPath -> Sh.Sh ()
buildTimingsWithGhc ghc = mkLogFileBy (ghcVersionWithGhc ghc) >>=
                          buildTimingsBy ["-w", unGhcPath ghc]


mkLogFileBy :: Sh.Sh T.Text -> Sh.Sh LogFile
mkLogFileBy = fmap (\version -> LogFile $ version <> "-" <> logFile)


mkLogFile :: Sh.Sh LogFile
mkLogFile = mkLogFileBy ghcVersion


mkLogFileWithGhc :: GhcPath -> Sh.Sh LogFile
mkLogFileWithGhc = mkLogFileBy . ghcVersionWithGhc

ghcVersion :: Sh.Sh T.Text
ghcVersion = T.strip <$> Sh.command "ghc" ["--numeric-version"] []


ghcVersionWithGhc :: GhcPath -> Sh.Sh T.Text
ghcVersionWithGhc (T.unpack . unGhcPath -> ghc) =
  T.strip <$> Sh.command ghc ["--numeric-version"] []


mkTimingFile :: Sh.Sh TimingsFile
mkTimingFile = (\version -> TimingsFile $ version <> "-" <> timingFile) <$> ghcVersion


mkCSVFile :: Sh.Sh CSVFile
mkCSVFile = (\version -> CSVFile $ version <> "-" <> csvFile) <$> ghcVersion


mkTimingFileWithGhc :: GhcPath -> Sh.Sh TimingsFile
mkTimingFileWithGhc = fmap (\version -> TimingsFile $ version <> "-" <> timingFile) . ghcVersionWithGhc


mkCSVFileWithGhc :: GhcPath -> Sh.Sh CSVFile
mkCSVFileWithGhc = fmap (\version -> CSVFile $ version <> "-" <> csvFile) . ghcVersionWithGhc


collectCSVs :: TimingsFile -> CSVFile -> Sh.Sh ()
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
