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

module Main where

import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Options.Applicative as O

import Data.Semigroup ((<>))

data Input = Input { package :: T.Text -- ^ name of input package
                   }                   -- other input flags here


-- | parse the input package and options
parseInput :: O.Parser Input
parseInput = Input <$>
             O.option O.str (  O.long "package"
                            <> O.short 'p'
                            <> O.metavar "PACKAGE"
                            <> O.help "package name"
                            )


main :: IO ()
main = do
  input <- O.execParser $ O.info (O.helper <*> parseInput) mempty
  TIO.putStrLn $ package input
