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

{-# OPTIONS_GHC -Wall -Werror     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module GHC.Utils where

import qualified Data.Text as T
-- import qualified Data.Text.IO        as TIO
import qualified Shelly    as Sh


import           GHC.Types


class Exists a where exists :: a -> Sh.Sh Bool
instance Exists CompressedPackage where exists = exists    .
                                                 T.unpack  .
                                                 unCompressedPackage
instance Exists PackageDirectory where exists = Sh.test_d . unPackageDirectory
instance Exists FilePath         where exists = Sh.test_f
instance Exists T.Text           where exists = Sh.test_f . T.unpack


-- | Convert a package and version to a URL
toUrl :: (Package, Version) -> URL
toUrl (p,v) = hackage <> T.pack (p Sh.</> p)
              <> dash <> T.pack (v Sh.<.> tarGz)
  where
    dash    = "-" :: T.Text
    hackage = "http://hackage.haskell.org/package/"


expand :: CompressedPackage -> PackageDirectory -> Sh.Sh ()
expand (unCompressedPackage -> package) (T.pack . unPackageDirectory -> target) =
  Sh.run_ "tar" ["-xvf", package, "-C", target]


cat :: T.Text -> Sh.Sh T.Text
cat = Sh.run "cat" . pure


toCompressedPackage :: Package -> CompressedPackage
toCompressedPackage (T.unpack -> p) = CompressedPackage . T.pack $
                                      workingDir Sh.</> tarCache Sh.</> p Sh.<.> tarGz

toPackageDirectory :: Package -> PackageDirectory
toPackageDirectory p = PackageDirectory $! workingDir Sh.</> projectCache Sh.</> p


wget :: [(Package, Version)] -> T.Text -> Sh.Sh ()
wget ps target = Sh.setStdin (T.unlines . fmap toUrl $ ps) >>
                 Sh.run_ "xargs" ["wget", "--directory-prefix=" <> target]


resetTarCache :: Sh.Sh ()
resetTarCache = Sh.rm_rf (workingDir Sh.</> tarCache)
                >> Sh.mkdir_p (workingDir Sh.</> tarCache)


createWorkingDir :: Sh.Sh ()
createWorkingDir = Sh.mkdir_p (T.unpack workingDir)
