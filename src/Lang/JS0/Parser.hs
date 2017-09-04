{-# LANGUAGE OverloadedStrings #-}

module Lang.JS0.Parser
where

import Lang.JS0.Prelude
import Lang.JS0.AST

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- ----------------------------------------
--
-- lexer primitives

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

rword :: Text -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

rws :: [Text] -- list of reserved words
rws =
  [ "abstract"
  , "boolean", "break", "byte"
  , "case", "catch", "char", "class", "const", "continue"
  , "debugger", "default", "delete", "do", "double"
  , "else", "enum", "export", "extends"
  , "false", "final", "finally", "float", "for", "function"
  , "goto"
  , "if", "implements", "import", "in", "instanceof", "int", "interface"
  , "long"
  , "native", "new", "null"
  , "package", "private", "protected", "public"
  , "return"
  , "short", "static", "super", "switch", "synchronized"
  , "this", "throw", "throws", "transient", "true", "try", "typeof"
  , "var", "volatile", "void"
  , "while", "with"
  ]

identifier :: Parser Text
identifier = (lexeme . try) (p >>= check)
  where
    p  :: Parser Text
    p  = do
      s <- (:) <$> letterChar <*> many alphaNumChar
      return $ s ^. isoText

    check :: Text -> Parser Text
    check x =
      if x `elem` rws
      then fail $ "keyword " ++ show x ++ " isn't allowed as identifier"
      else return x
