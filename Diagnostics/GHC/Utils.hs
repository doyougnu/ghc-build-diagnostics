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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module GHC.Utils where

import qualified Data.Text          as T
import qualified Shelly             as Sh

import           System.FilePath    (takeBaseName)
import           Data.Maybe         (isNothing)
-- import           Control.Monad      ((<=<))
-- import qualified Distribution.Package                    as P
import qualified Distribution.PackageDescription         as PD
import qualified Distribution.PackageDescription.Parsec  as Parse
import Distribution.Verbosity (normal)
-- import Distribution.Types.UnqualComponentName (UnqualComponentName)
-- import Distribution.Types.Dependency

import           GHC.Types


class Exists a where exists :: a -> Sh.Sh Bool
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
expand (unCompressedPackage -> package) target =
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
toPackageDirectory p = PackageDirectory . toText $! workingDir Sh.</> projectCache Sh.</> p


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


findInProject :: PackageDirectory -> T.Text -> Sh.Sh T.Text
findInProject (unPackageDirectory -> path) toFind =
  Sh.command "find" [path, "-name", toFind] []


findCabal :: PackageDirectory -> Sh.Sh CabalFile
findCabal path = findInProject path "*.cabal" >>= mkCabalFile


findMain :: PackageDirectory -> Sh.Sh (MainFile a)
findMain p = findCabal p >>= go >>= findInProject p >>= mkMainFile
  where go :: CabalFile -> Sh.Sh T.Text
        go (T.strip . unCabalFile -> cbl) =
          do desc <- Sh.liftIO $ Parse.readGenericPackageDescription normal (toPath cbl)
             let mn = head $ getMainExe desc
             return $ T.pack mn



isLibrary :: PD.GenericPackageDescription -> Bool -- Maybe (PD.CondTree PD.ConfVar [P.Dependency] PD.Library)
isLibrary = isNothing . PD.condLibrary


isExecutable :: PD.GenericPackageDescription -> Bool
isExecutable = null . PD.condExecutables


getMainExe :: PD.GenericPackageDescription -> [FilePath]
getMainExe = fmap (PD.modulePath . PD.condTreeData . snd) . PD.condExecutables


-- getMainLib :: PD.GenericPackageDescription -> [FilePath]
-- getMainLib = maybe mempty go . PD.condLibrary
--   where go = PD.modulePath . PD.condTreeData


getDependencies' :: PD.GenericPackageDescription -> [FilePath]
getDependencies' = fmap (PD.modulePath . PD.condTreeData . snd) . PD.condExecutables
