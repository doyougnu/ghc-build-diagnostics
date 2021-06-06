{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module    : Main
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Main module to generate diagnostics of ghc build times given an input package
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Options.Applicative as O
import qualified Shelly              as Sh

import           Control.Applicative ((<|>))
import           Data.Semigroup      ((<>))
import           Control.Arrow       (second)



data Mode = Packages Package -- ^ Analyze a set of packages, read from stdin
          | RefreshPList     -- ^ Refresh the package list
          | BuildCache       -- ^ Download all the packages from hackage
          | Clean            -- ^ Delete the cache


-- | constant for directory to hold packages
workingDir :: T.Text
workingDir = ".ghc-diagnostics"

-- | constant for the package cache
packageDir :: T.Text
packageDir = "packageCache"

-- | constant for the file to hold a list of packages
packageList :: T.Text
packageList = "package-list"

-- | Type synonyms for more descriptive types
type Package = T.Text
type Version = T.Text
type URL     = T.Text


main :: IO ()
main = do
  mode <- O.execParser $ O.info (O.helper <*> parseInput) mempty
  let between before after go = do TIO.putStrLn before
                                   go
                                   TIO.putStrLn after
  case mode of
    RefreshPList -> between "Refreshing package list" "all done" $
                    retrievePackageList workingDir packageList
    Clean        -> between "Cleaning..." "done" .
                    Sh.shelly $ Sh.rm_rf (T.unpack workingDir)
    BuildCache   -> between "Building cache" "done" retrieveAllPackages
    Packages ps  -> doPackages ps


-- | parse the input package and options
parseInput :: O.Parser Mode
parseInput = packageInput <|> refresh <|> clean <|> build
  where packageInput = Packages <$>
          O.option O.auto ( O.long    "package"
                            <> O.short   'p'
                            <> O.metavar "PACKAGE"
                            <> O.help    "package name"
                          )

        refresh = O.flag' RefreshPList ( O.long "update"
                                         <> O.short 'u'
                                         <> O.help "Update the list of cabal packages"
                                       )

        clean   = O.flag' Clean (  O.long "clean"
                                <> O.short 'c'
                                <> O.help "clean up the working directory"
                                )

        build   = O.flag' BuildCache (    O.long "build"
                                       <> O.short 'b'
                                       <> O.help "rebuild the entire package cache"
                                     )


-- | Convert a package and version to a URL
toUrl :: (Package, Version) -> URL
toUrl (p,v) = hackage <> T.pack (p Sh.</> p) <> dash <> T.pack (v Sh.<.> tarGz)
  where
    dash    = "-" :: T.Text
    tarGz   = T.pack $! ("tar" :: T.Text) Sh.<.> "gz"
    hackage = "http://hackage.haskell.org/package/"


-- | get a list of packages from cabal
retrievePackageList :: T.Text -> T.Text -> IO ()
retrievePackageList dir listName =
  do ps <- Sh.shelly $
       do packages <- Sh.silently $ Sh.run "cabal" ["list", "--simple"]
          return . fmap (toUrl . second T.tail . T.breakOn " ") . T.lines $ packages

     -- make the working directory
     Sh.shelly $ Sh.mkdir_p (T.unpack dir)
     TIO.writeFile (dir Sh.</> listName) (T.unlines ps)


-- | Download all packages in the package list from hackage
retrieveAllPackages :: IO ()
retrieveAllPackages = do ps <- TIO.readFile (workingDir Sh.</> packageList)
                         let wd       = T.unpack workingDir
                             pd       = T.unpack packageDir
                             arg      = "--input-file=" :: T.Text
                             get file = Sh.run_ "wget" $
                                        pure . T.pack $ arg Sh.</> file
                         Sh.shelly $
                           do Sh.rm_rf (wd Sh.</> pd) >> Sh.mkdir_p (wd Sh.</> pd)
                              root <- Sh.pwd
                              get (root Sh.</> wd Sh.</> packageList)


doPackages :: T.Text -> IO ()
doPackages (T.lines -> ps) = undefined
