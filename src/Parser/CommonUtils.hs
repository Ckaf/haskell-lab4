module Parser.CommonUtils (
absorbToStopWord, regularParse, whitespace, lexeme, absorbToStopWordNoRead,
Option(..), Utils(..), UtilsDiff(..), UtilsOrDiff(..), word, wordWithoutLexem, syntaxExpr,
findDiffOptions, findArgs
) where
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, anyChar, oneOf, char, letter, digit)
import Control.Applicative (liftA2, (<|>), many)
import Text.Parsec.Prim (parse, try, lookAhead)
import Text.Parsec.Error (ParseError)
import Control.Monad (void)
import Data.List (sort, find)

word :: Parser String
word =  lexeme ((:) <$> firstChar <*> many nonFirstChar)
        where
          firstChar = letter <|> char '-' <|> char '_' 
          nonFirstChar = digit <|> firstChar <|> char '.'

wordWithoutLexem :: Parser String
wordWithoutLexem =  (:) <$> firstChar <*> many nonFirstChar
        where
          firstChar = letter <|> char '-' <|> char '_' 
          nonFirstChar = digit <|> firstChar <|> char '.' 
          



bracketExpr :: Parser Option -- todo need fix [[]]
bracketExpr = do
  void $ lexeme $ char '['
  void $ many $ char '-' -- ignore -
  name <- word -- option name
  args <- many word
  void $ lexeme $ char ']'
  if null args then return $ Option name Nothing Nothing
  else return $ Option name (Just args) Nothing

syntaxExpr :: Parser Utils
syntaxExpr = do
  name <- word
  opts <- many bracketExpr
  if null opts then return $ Utils name Nothing Nothing
  else return $ Utils name (Just opts) Nothing
  
absorbToStopWord :: String -> Parser String
absorbToStopWord stopWord = try ("" <$ string stopWord)
               <|> liftA2 (:) anyChar (absorbToStopWord stopWord)

-- that's for debug
regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

absorbToStopWordNoRead :: String -> Parser String
absorbToStopWordNoRead stopWord = lookAhead ("" <$ string stopWord)
               <|> liftA2 (:) anyChar (absorbToStopWordNoRead stopWord)

data Option = Option {optName:: String, optArgs :: Maybe [String],
                    optDescription :: Maybe String } deriving Show

data Utils = Utils {utilName :: String, options :: Maybe [Option],
 utilDescription :: Maybe String}
  deriving Show

data UtilsDiff = UtilsDiff {blockOptions1 :: [String], blockOptions2 :: [String]}
  deriving Show

data UtilsOrDiff = Ut Utils | Diff UtilsDiff
  deriving Show
  
findDiffOptions :: Maybe [Option] -> Maybe [Option] ->  Maybe UtilsDiff
findDiffOptions Nothing Nothing = Nothing
findDiffOptions (Just optsList1) Nothing = do
  let optsNames1 = foldl (\list opt -> optName opt : list) [] optsList1
  Just $ UtilsDiff (sort optsNames1) []
  
findDiffOptions Nothing (Just optsList2)  = do
  let optsNames2 = foldl (\list opt -> optName opt : list) [] optsList2
  Just $ UtilsDiff (sort optsNames2) []
  
findDiffOptions (Just optsList1) (Just optsList2) = do
  let optsNames1 = foldl (\list opt -> optName opt : list) [] optsList1
  let optsNames2 = foldl (\list opt -> optName opt : list) [] optsList2
  if sort optsNames1 == sort optsNames2 then Nothing
  else Just $ UtilsDiff (sort optsNames1) (sort optsNames2)
  
findArgs :: Option -> Maybe [Option] -> Maybe [String]
findArgs _ Nothing = Nothing
findArgs op (Just ops) = do
  cur_op <- find (\e -> optName e == optName op) ops
  let args = optArgs cur_op
  args <|> Nothing