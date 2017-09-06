{-# LANGUAGE OverloadedStrings #-}

module Lang.JS0.Parser
where

import Data.Char
import Data.Scientific

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

keyword :: Text -> Parser ()
keyword w = lexeme (string w *> notFollowedBy nameChar)

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
  , "NaN"
  , "native", "new", "null"
  , "package", "private", "protected", "public"
  , "return"
  , "short", "static", "super", "switch", "synchronized"
  , "this", "throw", "throws", "transient", "true", "try", "typeof"
  , "undefined"
  , "var", "volatile", "void"
  , "while", "with"
  ]

kw'abstract
  , kw'boolean, kw'break, kw'byte
  , kw'case, kw'catch, kw'char, kw'class, kw'const, kw'continue
  , kw'debugger, kw'default, kw'delete, kw'do, kw'double
  , kw'else, kw'enum, kw'export, kw'extends
  , kw'false, kw'final, kw'finally, kw'float, kw'for, kw'function
  , kw'goto
  , kw'if, kw'implements, kw'import, kw'in, kw'instanceof, kw'int, kw'interface
  , kw'long
  , kw'NaN
  , kw'native, kw'new, kw'null
  , kw'package, kw'private, kw'protected, kw'public
  , kw'return
  , kw'short, kw'static, kw'super, kw'switch, kw'synchronized
  , kw'this, kw'throw, kw'throws, kw'transient, kw'true, kw'try, kw'typeof
  , kw'undefined
  , kw'var, kw'volatile, kw'void
  , kw'while, kw'with
  :: Parser ()

[ kw'abstract
  , kw'boolean, kw'break, kw'byte
  , kw'case, kw'catch, kw'char, kw'class, kw'const, kw'continue
  , kw'debugger, kw'default, kw'delete, kw'do, kw'double
  , kw'else, kw'enum, kw'export, kw'extends
  , kw'false, kw'final, kw'finally, kw'float, kw'for, kw'function
  , kw'goto
  , kw'if, kw'implements, kw'import, kw'in, kw'instanceof, kw'int, kw'interface
  , kw'long
  , kw'NaN
  , kw'native, kw'new, kw'null
  , kw'package, kw'private, kw'protected, kw'public
  , kw'return
  , kw'short, kw'static, kw'super, kw'switch, kw'synchronized
  , kw'this, kw'throw, kw'throws, kw'transient, kw'true, kw'try, kw'typeof
  , kw'undefined
  , kw'var, kw'volatile, kw'void
  , kw'while, kw'with
  ] = map keyword rws

nameChar :: Parser Char
nameChar = alphaNumChar <|> otherNameChar

otherNameChar :: Parser Char
otherNameChar = satisfy (\ x -> x == '_' || x == '$')

name :: Parser Text
name = (lexeme . try) (p >>= check)
  where
    p  :: Parser Text
    p  = do
      c <- letterChar <|> otherNameChar
      s <- many (alphaNumChar <|> otherNameChar)
      return $ (c : s) ^. isoText

    check :: Text -> Parser Text
    check x =
      if x `elem` rws
      then fail $ "keyword " ++ show x ++ " isn't allowed as identifier"
      else return x

numberLiteral :: Parser Double
numberLiteral =
  L.scientific >>= return . toRealFloat

sQuote, dQuote :: Parser Char
sQuote = satisfy (== '\'')
dQuote = satisfy (== '"')

anyLitChar :: (Char -> Bool) -> Parser Char
anyLitChar q =
  satisfy (\ x -> q x
                  &&
                  x /= '\\'
                  &&
                  not (isControl x)
          )
  <|>
  ( char '\\'
    >>
    escapedChar
  )

escapedChar :: Parser Char
escapedChar =
  satisfy (\ x -> x == '"'
                  ||
                  x == '\''
                  ||
                  x == '\\'
                  ||
                  x == '/'
          )
  <|>
  ( char 'b' >> return '\b' )
  <|>
  ( char 'f' >> return '\f' )
  <|>
  ( char 'n' >> return '\n' )
  <|>
  ( char 'r' >> return '\r' )
  <|>
  ( char 't' >> return '\t' )
  <|>
  hexDigits4

hexDigits4 :: Parser Char
hexDigits4 = do
  c4 <- count 4 hexDigitChar
  return $ toEnum $ foldl (\ r i -> r * 16 + i) 0 $ map digitToInt c4

stringLiteral :: Parser Text
stringLiteral = (^. isoText) <$> (singleQ <|> doubleQ)
  where
    singleQ = between sQuote sQuote $ many $ anyLitChar (/= '\'')
    doubleQ = between dQuote dQuote $ many $ anyLitChar (/= '"')

op'incr
  , op'decr
  , dot
  , colon
  , comma
  , semicolon
  , qumark
  , leftPar
  , rightPar
  , leftBracket
  , rightBracket
  , leftBrace
  , rightBrace :: Parser Text

[ op'incr
  , op'decr
  , dot
  , colon
  , comma
  , semicolon
  , qumark
  , leftPar
  , rightPar
  , leftBracket
  , rightBracket
  , leftBrace
  , rightBrace
  ] = map symbol
      [  "+=", "-="
      , ".", ":", ",", ";", "?"
      , "(", ")", "[", "]", "{", "}"
      ]

op'assign
  , op'plus
  , op'minus
  , op'not :: Parser Text

[ op'assign
  , op'plus
  , op'minus
  , op'not
  ] = map (\ s -> lexeme $ string s <* notFollowedBy (char '='))
      [ "=", "+", "-", "!"]

-- ----------------------------------------
--
-- statement parser

varStatements :: Parser [Stmt]
varStatements = mconcat <$> many varStatement

varStatement :: Parser [Stmt]
varStatement = kw'var *> vars <* semicolon
  where
    vars = sepBy1 var1 comma
    var1 = mkVarStmt <$> name <*> option mkNull (op'assign *> expression)

statements :: Parser [Stmt]
statements = many statement

statement :: Parser Stmt
statement =
  labeledStatement
  <|>
  (expressionStatement <* semicolon)
  <|>
  disruptiveStatement
  <|>
  tryStatement
  <|>
  ifStatement
  <|>
  compoundStatement

labeledStatement :: Parser Stmt
labeledStatement =
  mkLabelStmt <$>
  try (name <* colon) <*> compoundStatement

disruptiveStatement :: Parser Stmt
disruptiveStatement =
  breakStatement
  <|>
  returnStatement
  <|>
  throwStatement

breakStatement :: Parser Stmt
breakStatement =
  mkBreakStmt <$>
  (kw'break *> optional name <* semicolon)

returnStatement :: Parser Stmt
returnStatement =
  mkReturnStmt <$>
  (kw'return *> optional expression <* semicolon)

throwStatement :: Parser Stmt
throwStatement =
  mkThrowStmt <$>
  (kw'throw *> expression)

tryStatement :: Parser Stmt
tryStatement =
  mkTryStmt <$>
  (kw'try *> block <* kw'catch) <*> (leftPar *> name <* rightPar) <*> block

ifStatement :: Parser Stmt
ifStatement =
  mkIfStmt <$>
  (kw'if *> parExpression) <*> thenPart <*> elsePart
  where
    thenPart = block
    elsePart =
      (kw'else *> (ifStatement <|> block))
      <|>
      pure (mkStmtSeq [])

compoundStatement :: Parser Stmt
compoundStatement =
  switchStatement
  <|>
  whileStatement
  <|>
  forStatement
  <|>
  doStatement

switchStatement :: Parser Stmt
switchStatement = undefined

whileStatement :: Parser Stmt
whileStatement =
  mkWhileStmt <$>
  (kw'while *> parExpression) <*> block

forStatement :: Parser Stmt
forStatement = undefined

doStatement :: Parser Stmt
doStatement =
  mkDoStmt <$>
  (kw'do *> block <* kw'while) <*> (parExpression <* semicolon)

block :: Parser Stmt
block =
  between leftBrace rightBrace (mkStmtSeq <$> statements)

-- the grammar given in JavaScript: The Good Parts can't be parsed
-- with a limited look ahead
--
-- the expression statement rule on page 14 is pretty tricky
--
-- example:
-- x = f(42).y = 23   -- o.k.
-- x = f(42)   = 23   -- wrong
-- x = f(42)          -- o.k.

expressionStatement :: Parser Stmt
expressionStatement =
  mkExprStmt <$>
  ( deleteExpression
    <|>
    assignOrCall
  )

assignOrCall :: Parser Expr
assignOrCall = do
  lhs <- ident >>= selectors
  if isLValue lhs
    then assignment lhs
    else return lhs

assignment :: Expr -> Parser Expr
assignment lhs =
  (op'incr   *> (mkIncr lhs <$> expression))
  <|>
  (op'decr   *> (mkDecr lhs <$> expression))
  <|>
  (op'assign *> (mkAssign lhs <$> assignment'))
  where
    assignment' =
      try assignOrCall    -- backtracking necessary
      <|>
      expression

-- ----------------------------------------

expression :: Parser Expr
expression = expr4

parExpression :: Parser Expr
parExpression = between leftPar rightPar expression

deleteExpression :: Parser Expr
deleteExpression =
  mkDelete <$>
  (kw'delete *> refinementExpression)

refinementExpression :: Parser Expr
refinementExpression =
  expression >>= checkRefinement
  where
    checkRefinement :: Expr -> Parser Expr
    checkRefinement e
      | isRefinement e = return e
      | otherwise      = fail "expression with refinement expected (\"expr.name\" or \"expr[expr]\")"

invocationExpression :: Parser Expr
invocationExpression =
  expression >>= checkInvocation
  where
    checkInvocation e
      | isInvocation e = return e
      | otherwise      = fail "invocation expression expected (\"expr(args)\")"

selectors :: Expr -> Parser Expr
selectors e =
  (dot *> (mkDotExpr e <$> name) >>= selectors)
  <|>
  ((leftBracket *> (mkBoxExpr e <$> expression) <* rightBracket) >>= selectors)
  <|>
  ((leftPar *> (mkCallExpr e <$> sepBy expression comma) <* rightPar ) >>= selectors)
  <|>
  pure e

ident :: Parser Expr
ident = mkIdent <$> name

-- literals, names and expressions enclosed in ( and )
expr0 :: Parser Expr
expr0 =
  ident
  <|>
  literal
  <|>
  (kw'null      *> pure mkNull)
  <|>
  (kw'undefined *> pure mkUndef)
  <|>
  (leftPar *> expression <* rightPar)

-- add selectors: field, index and call
expr1 :: Parser Expr
expr1 = expr0 >>= selectors

-- add unary operators
expr2 :: Parser Expr
expr2 =
  (mkUplus  <$> (op'plus  *> expr2))
  <|>
  (mkUminus <$> (op'minus *> expr2))
  <|>
  (mkNot    <$> (op'not   *> expr2))
  <|>
  (mkTypeof <$> (kw'typeof *> expr2))
  <|>
  (mkNew    <$> (kw'new *> invocationExpression))
  <|>
  deleteExpression
  <|>
  expr1

-- add binary operators
expr3 :: Parser Expr
expr3 = makeExprParser expr2 operators

operators :: [[Operator Parser Expr]]
operators =
  [ [ InfixL (mkMult <$ symbol "*")
    , InfixL (mkDiv  <$ symbol "/")
    , InfixL (mkRem  <$ symbol "%")
    ]
  , [ InfixL (mkAdd  <$ op'plus )
    , InfixL (mkSub  <$ op'minus)
    ]
  , [ InfixL (mkGE   <$ symbol ">=")
    , InfixL (mkGR   <$ symbol ">" )
    , InfixL (mkLE   <$ symbol "<=")
    , InfixL (mkLS   <$ symbol "<" )
    ]
  , [ InfixL (mkEQ   <$ symbol "===")
    , InfixL (mkNE   <$ symbol "!==")
    ]
  , [ InfixR (mkAnd  <$ symbol "&&")
    ]
  , [ InfixR (mkOr   <$ symbol "||")
    ]
  ]

-- add conditional expressions
expr4 :: Parser Expr
expr4 = do
  e1 <- expr3
  ( ( mkCond e1 <$>
      (qumark *> expr3) <*> (colon *> expr4)
    )
    <|>
    return e1
    )

-- ----------------------------------------

literal :: Parser Expr
literal =
  (mkStrLit <$> stringLiteral)
  <|>
  (mkNumLit <$> numberLiteral)
  <|>
  (kw'NaN *> pure (mkNumLit $ 0.0/0.0))
  <|>
  objectLiteral

objectLiteral :: Parser Expr
objectLiteral = undefined

-- ----------------------------------------
