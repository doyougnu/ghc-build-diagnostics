-----------------------------------------------------------------------------
-- |
-- Module    : GHC.Process
-- Copyright : (c) Jeffrey Young
-- License   : BSD3
-- Maintainer: youngjef@oregonstate.edu
-- Stability : experimental
--
-- Module for parsing timings and translating to csv
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}



import           Text.Megaparsec
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Csv                   as CSV
import qualified Data.Text                  as T

import           Control.Monad              (void,msum)
import           Data.Either                (rights)
import           Data.Functor               (($>))
import           Data.Text.IO               (readFile)
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import           Prelude                    hiding (readFile, unwords,lines)

import           GHC.Types                  hiding (empty)
import qualified GHC.Utils                  as U


type Parser = Parsec Void T.Text

timingsToCsv :: ToPath a => a -> IO [Row]
timingsToCsv file = do contents <- readFile . toPath $ file
                       let
                         ls :: [T.Text]
                         ls = T.lines contents
                       return $ rights $ fmap (parse row mempty) ls


sc :: Parser ()
sc = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

startingExcls :: Parser T.Text
startingExcls = lexeme $ symbol "!!!"

lBracket :: Parser T.Text
lBracket = symbol "["

rBracket :: Parser T.Text
rBracket = symbol "]"

colon :: Parser T.Text
colon = symbol ":"

comma :: Parser T.Text
comma = symbol ","

letters :: Parser (Token T.Text)
letters = lexeme C.letterChar

phase :: Parser T.Text
phase = T.unwords . fmap T.pack <$> someTill (C.letterChar `endBy1` C.space) lBracket

module' :: Parser T.Text
module' = T.pack <$> someTill (C.alphaNumChar <|> C.punctuationChar) rBracket

time :: Parser Float
time = lexeme $ skipSomeTill letters (lexeme L.float)

alloc :: Parser Float
alloc = skipSomeTill letters comma >> skipSomeTill letters (lexeme L.float)

row :: Parser Row
row = do _ <- startingExcls
         _phase  <- phase
         _module <- module'
         void colon
         _time   <- time
         _alloc  <- alloc
         return Row{..}


data Row = Row { _phase  :: !T.Text
               , _module :: !T.Text
               , _time   :: !Float
               , _alloc  :: !Float
               } deriving (Generic, Show)


instance CSV.ToNamedRecord  Row
instance CSV.DefaultOrdered Row

t :: T.Text
t = "!!! initializing package database: finished in 22.98 milliseconds, allocated 15.857 megabytes"
