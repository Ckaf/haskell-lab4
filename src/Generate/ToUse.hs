{-# LANGUAGE BlockArguments #-}

module Generate.ToUse(
utilToUse
) where

import Parser.CommonUtils

getOptionsInBrackets :: [Option] -> String
getOptionsInBrackets = foldl(\s e -> do
  let s' = s ++ "[ -" ++ optName e ++ " "
  let s'' = maybe "" unwords (optArgs e)
  s' ++ s''++ "]"
  ) "" 

syntaxUse :: Utils -> String
syntaxUse ut = do
  let str = utilName ut
  let opts = maybe "" getOptionsInBrackets (options ut)                
  str ++ opts
    
optionsUse :: Utils -> String
optionsUse ut = maybe "" (
  foldl (
    \s o -> s ++ "-" ++ optName o ++ "\t" ++ 
    maybe "" id (optDescription o) ++ "\n"
     ) ""
  ) (options ut)  
  
utilToUse :: Utils -> String
utilToUse ut = do
  let s' = syntaxUse ut
  let s'' = optionsUse ut
  s' ++ "\nOptions:\n" ++ s'' ++ "\nDescription:\n" 
  ++ maybe "" id (utilDescription ut)

    
   