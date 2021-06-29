{-# LANGUAGE RecordWildCards #-}
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



import           Text.Megaparsec
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

import           Control.Monad              (void)
import           Data.Functor               (($>))
import           Prelude                    hiding (unwords)
import           Data.Text                  hiding (empty)
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)

import qualified GHC.Utils                  as U

type Parser = Parsec Void Text

-- parseLine :: Text -> Row
parseLine s = run
  where run =  parse go "" s

go :: Parser [Row]
go = between sc eof (row `sepEndBy` C.newline)

sc :: Parser ()
sc = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

startingExcls :: Parser Text
startingExcls = lexeme $ symbol "!!!"

lBracket :: Parser Text
lBracket = symbol "["

rBracket :: Parser Text
rBracket = symbol "]"

colon :: Parser Text
colon = symbol ":"

comma :: Parser Text
comma = symbol ","

letters :: Parser (Token Text)
letters = lexeme C.letterChar

phase :: Parser Text
phase = unwords . fmap pack <$> someTill (C.letterChar `endBy1` C.space) lBracket

module' :: Parser Text
module' = pack <$> someTill (C.alphaNumChar <|> C.punctuationChar) rBracket

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



data Row = Row { _phase  :: !Text
               , _module :: !Text
               , _time   :: !Float
               , _alloc  :: !Float
               } deriving (Generic, Show)
