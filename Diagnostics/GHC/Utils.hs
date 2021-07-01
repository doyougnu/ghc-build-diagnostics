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

import qualified Data.Text                              as T
import qualified Shelly                                 as Sh

import           Control.Exception.Base                 (SomeException)
import           Data.List                              ((\\))
import           Data.Maybe                             (isNothing)
import           System.FilePath                        (takeBaseName)
import           Data.Functor                           ((<&>))

import qualified Distribution.PackageDescription        as PD


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
findIn :: T.Text -- ^ Where to look
       -> T.Text -- ^ thing to find
       -> [T.Text] -- ^ Extra commands
       -> Sh.Sh T.Text
findIn here thing = go
  where go = Sh.command "find" [here, "-name", thing]


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
cabalGet p = Sh.catch_sh go handle -- cabal will error if the directory already
                                   -- exists we catch this error here to
                                   -- continue processing the directory and auto
                                   -- succeed if the directory exists
  where
    go :: Sh.Sh ()
    go = Sh.command_ "cabal" ["get"] [p]
    handle :: SomeException -> Sh.Sh()
    handle _  = return ()

cabalBuild :: LogFile -> [T.Text] -> Sh.Sh ()
cabalBuild lf = Sh.escaping False .
                Sh.command_ "cabal" [ "new-build"
                                    , "--allow-newer"
                                    , "--ghc-option=-ddump-timings"
                                    , "--ghc-option=-v2"
                                    , "2>&1" -- to capture symbols sent to stderr
                                    , "|"
                                    , "tee"
                                    , lf
                                    ]


cachedPackages :: Sh.Sh PackageSet
cachedPackages = PackageSet . fmap packageName <$> Sh.lsT cache
  where
    packageName :: Package -> Package
    packageName = fst . T.breakOn dash . T.pack . takeBaseName . T.unpack
    dash        = "-" :: T.Text


validatePackages :: PackageSet -> Sh.Sh RebuildSet
validatePackages (unPackageSet -> ps) =
  do cachedPs <- unPackageSet <$> cachedPackages
     return . RebuildSet $ ps \\ cachedPs


buildTimingsBy :: [T.Text] -- ^ Extra arguments
               -> LogFile  -- ^ the log file name
               -> Sh.Sh ()
buildTimingsBy = flip cabalBuild


mkLogFileBy :: Sh.Sh T.Text -> Sh.Sh T.Text
mkLogFileBy getVersion = (\version -> version <> "-" <> logFile) <$> getVersion


mkLogFile :: Sh.Sh T.Text
mkLogFile = mkLogFileBy ghcVersion


ghcVersion :: Sh.Sh T.Text
ghcVersion = T.strip <$> Sh.command "ghc" ["--numeric-version"] []


ghcVersionWithGhc :: GhcPath -> Sh.Sh T.Text
ghcVersionWithGhc (T.unpack . unGhcPath -> ghc) =
  T.strip <$> Sh.command ghc ["--numeric-version"] []


mkTimingFile :: Sh.Sh T.Text
mkTimingFile = (\version -> version <> "-" <> timingFile) <$> ghcVersion


buildTimings :: Sh.Sh ()
buildTimings = mkLogFileBy ghcVersion >>= buildTimingsBy mempty


buildTimingsWithGhc :: GhcPath -> Sh.Sh ()
buildTimingsWithGhc ghc = mkLogFileBy (ghcVersionWithGhc ghc) >>=
                          buildTimingsBy ["-w", unGhcPath ghc]
