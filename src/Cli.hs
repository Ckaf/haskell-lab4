{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Strict #-}

module Cli
  (
  Status, parseInput
  )
where

import System.Console.CmdArgs
import Parser.Dox
import Parser.Use
import Parser.CommonUtils
import Data.Either
import Generate.ToMd
import Generate.ToUse


import Text.Parsec.Prim (runParser)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (parseFromFile)

import System.IO
import Data.Char (toUpper)
--import Text.Parsec.ByteString.Lazy (parseFromFile)

data IOptions = IOptions
  { dox :: String,
    use :: String,
    md :: String,
    gu :: Bool
  }
  deriving (Show, Data, Typeable)

inputOptions :: IOptions
inputOptions =
  IOptions
    { md = "diff.md" &= help "Set the name of the output markdown file (by default diff.md)",
      dox = "" &= help "Path to dox file",
      use = "" &= help "Path to use file",
      gu = False &= help "Generates use file (generated.use)"
    }
    
data Status = OK | Failed | Md | Us deriving Show

parseInputC :: IOptions -> IO Status
parseInputC (IOptions "" "" _ _) = return Failed
parseInputC (IOptions doxPath "" mdPath fg) = do
 pr <- parseFromFile doxBlockExpression doxPath
 let pr' = pr
 case pr' of
      (Left m) -> do
        print m
        return Failed
      (Right resultParse) -> do
        case resultParse of
          Ut u -> case fg of
            False -> return OK
            True -> do
              generateUseFile u
              return Us
          Diff d -> do
            let dMd = diffMd "dox syntax" "dox options" d
            appendFile mdPath dMd
            return Md
 
parseInputC (IOptions "" usePath mdPath _) = do
   pr <- parseFromFile useBlockExpr usePath
   let pr' = pr
   case pr' of
        (Left m) -> do 
          print m
          return Failed
        (Right resultParse) -> do
          case resultParse of
            Ut _ -> return OK
            Diff d -> do
              let dMd = diffMd "use syntax" "use options" d
              appendFile mdPath dMd
              return Md
              
parseInputC (IOptions doxPath usePath mdPath fg) = do
  parseInputC (IOptions "" usePath mdPath fg)
  prU' <- parseFromFile useBlockExpr usePath
  let prU = prU'
  prD' <- parseFromFile doxBlockExpression doxPath
  let prD = prD'
  case [prU,prD] of
    [Right (Ut a), Right (Ut b)] -> do
      let diffs = findDiffOptions (options a) ( options b)
      let dMd = maybe "" (diffMd "use syntax" "use options") diffs
      appendFile mdPath dMd
      return Md
    _ -> return Failed
  parseInputC (IOptions doxPath "" mdPath fg)
  
  

parseInput :: IO Status
parseInput = do
  input <- cmdArgs inputOptions
  let input' = input
  writeFile (md input) ""
  let status = parseInputC input'
  status
      
generateUseFile :: Utils -> IO ()
generateUseFile ul = do
  let str = utilToUse ul
  writeFile "generated.use" str
