{-# LANGUAGE BlockArguments #-}

module Generate.ToUse
  ( utilToUse,
  )
where

import Data.Maybe (fromMaybe)
import Parser.CommonUtils
import Data.List (isPrefixOf)
import Data.List.Split (split, oneOf)

getOptionsInBrackets :: [Option] -> String
getOptionsInBrackets =
  foldl
    ( \s e -> do
        let s' = concat [s, "[ -", optName e, " "]
        let s'' = maybe "" unwords (optArgs e)
        concat [s', s'', "]"]
    )
    ""

syntaxUse :: Utils -> String
syntaxUse ut = do
  let str = utilName ut
  let opts = maybe "" getOptionsInBrackets (options ut)
  str ++ opts

optionsUse :: Utils -> String
optionsUse ut =
  maybe
    ""
    ( foldl
        ( \s o ->
            concat
              [ s,
                "-",
                optName o,
                "\t",
                fromMaybe "" (optDescription o),
                "\n"
              ]
        )
        ""
    )
    (options ut)

removeSpecialWords :: Maybe String -> String
removeSpecialWords str= do
  let s = Data.Maybe.fromMaybe "" str
  let s2 = split (oneOf "\t\n ") s
  concat $ filter (not . isPrefixOf "@") s2
  
utilToUse :: Utils -> String
utilToUse ut = do
  let s' = syntaxUse ut
  let s'' = optionsUse ut
  concat [s', "\nOptions:\n", s'', "\nDescription:\n",  removeSpecialWords (utilDescription ut)]
