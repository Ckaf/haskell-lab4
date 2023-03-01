module Parser.Use
  ( useBlockExpr,
  )
where

import Control.Applicative (many)
import Control.Monad (void)
import Data.Maybe ()
import Parser.CommonUtils
import Text.Parsec.Char (anyChar, char, noneOf, oneOf, string)
import Text.Parsec.Combinator (many1)
import Text.Parsec.String (Parser)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
  if take (length find) s == find
    then repl ++ replace (drop (length find) s) find repl
    else head s : replace (tail s) find repl

textWithIndent :: Int -> Parser String
textWithIndent spaces_num = do
  spaces <- many1 $ oneOf " \t"
  let repl_sp = replace spaces "\t" "    "
  let delta = length repl_sp - spaces_num
  if delta > 1
    then return ""
    else do
      str1 <- many1 $ noneOf "\n"
      void $ many1 $ char '\n'
      str2 <- many $ textWithIndent spaces_num
      let strm2 = unwords str2
      return $ str1 <> strm2

optionsDescriptionExpr :: Parser Option
optionsDescriptionExpr = do
  dash <- many $ char '-'
  name <- wordWithoutLexem -- option name
  spaces <- many1 $ oneOf " \t"
  let repl_sp = replace spaces "\t" "    "
  description1 <- many1 $ noneOf "\n"
  void $ many $ oneOf " \t"
  void $ char '\n'
  let indent = length repl_sp + length dash + length name
  description2 <- many $ textWithIndent indent
  let str_desc2 = unwords description2
  return $ Option name Nothing $ Just (description1 <> str_desc2)

optionsBlockExpr :: Parser [Option]
optionsBlockExpr = do
  void $ lexeme $ string "Options:"
  many optionsDescriptionExpr

useBlockExpr :: Parser UtilsOrDiff
useBlockExpr = do
  u1 <- syntaxExpr
  opts <- optionsBlockExpr
  void $ absorbToStopWord "Description:"
  utilDesc <- many anyChar
  case options u1 of
    Nothing -> return $ Diff (UtilsDiff [] (foldl (\list opt -> optName opt : list) [] opts))
    Just val -> do
      case findDiffOptions (Just val) (Just opts) of
        Nothing -> do
          let res_opt = map (\op -> op {optArgs = findArgs op (options u1)}) opts
          return $ Ut $ Utils (utilName u1) (Just res_opt) (Just utilDesc)
        Just val2 -> return $ Diff val2
