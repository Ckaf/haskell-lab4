module Parser.Dox
  ( doxBlockExpression,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Maybe ()
import Parser.CommonUtils
import Text.Parsec.Char (anyChar, char, string)
import Text.Parsec.Combinator (choice, many1, manyTill)
import Text.Parsec.Prim (try)
import Text.Parsec.String (Parser)

-- option parsers

paramKeyword :: Parser String
paramKeyword = lexeme $ string "@param"

paramExpr1 :: Parser Option -- @param -p
paramExpr1 = do
  void paramKeyword
  void $ lexeme $ char '-'
  paramName <- word
  return $ Option paramName Nothing Nothing

paramExpr2 :: Parser Option -- @param \--p
paramExpr2 = do
  void paramKeyword
  void $ lexeme $ string "\\--"
  paramName <- word
  return $ Option paramName Nothing Nothing

paramExpr3 :: Parser Option -- @param -p, argument
paramExpr3 = do
  prm <- paramExpr1
  void $ lexeme $ char ','
  arg <- word
  return prm {optArgs = Just [arg]}

paramExpr4 :: Parser Option -- @param \--p, argument
paramExpr4 = do
  prm <- paramExpr2
  void $ lexeme $ char ','
  arg <- word
  return prm {optArgs = Just [arg]}

paramExpr5 :: Parser Option -- @param -p=argument
paramExpr5 = do
  prm <- paramExpr1
  void $ lexeme $ char '='
  arg <- word
  return prm {optArgs = Just [arg]}

paramExpr6 :: Parser Option -- @param \--p=argument
paramExpr6 = do
  prm <- paramExpr2
  void $ lexeme $ char '='
  arg <- word
  return prm {optArgs = Just [arg]}

paramExpr :: Parser Option
paramExpr =
  choice
    [ try paramExpr6,
      try paramExpr5,
      try paramExpr4,
      try paramExpr3,
      try paramExpr2,
      try paramExpr1
    ]

paramBlockExpr :: Parser Option
paramBlockExpr = do
  p <- paramExpr
  description <- try (absorbToStopWordNoRead "@param") <|> absorbToStopWordNoRead "@par"
  return $ p {optDescription = Just description}

paramsBlockExpr :: Parser [Option]
paramsBlockExpr = do
  void $ absorbToStopWordNoRead "@param"
  many1 $ lexeme paramBlockExpr

-- parse dox syntax
syntaxDoxExpr :: Parser Utils
syntaxDoxExpr = do
  void $ lexeme $ absorbToStopWord "@remark"
  syntaxExpr

-- parse util description
utilDescriptionExpr :: Parser String
utilDescriptionExpr = do
  void $ manyTill (lexeme $ absorbToStopWord "@par") (lexeme $ string "Описание:")
  manyTill anyChar (try $ lexeme $ string "@par")

--  return $ concat t

doxBlockExpression :: Parser UtilsOrDiff
doxBlockExpression = do
  ut <- syntaxDoxExpr
  opts <- paramsBlockExpr
  description <- utilDescriptionExpr
  case options ut of
    Nothing -> return $ Diff (UtilsDiff [] (foldl (\list opt -> optName opt : list) [] opts))
    Just val -> do
      case findDiffOptions (Just val) (Just opts) of
        Nothing -> do
          let res_opt = map (\op -> op {optArgs = findArgs op (options ut)}) opts
          return $ Ut $ Utils (utilName ut) (Just res_opt) (Just description)
        Just val2 -> return $ Diff val2
