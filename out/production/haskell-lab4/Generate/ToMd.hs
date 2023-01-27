module Generate.ToMd
  ( diffMd,
  )
where

import Data.List
import Parser.CommonUtils

interweave :: String -> [String] -> [String] -> (Int, Int) -> String
interweave s [] (y : ys) cls =
  s ++ "| " ++ replicate (fst cls) ' ' ++ "|"
    ++ replicate (floor $ fromIntegral (snd cls - length y) / 2) ' '
    ++ y
    ++ replicate (ceiling $ fromIntegral (snd cls - length y) / 2) ' '
    ++ " | "
    ++ "  Failed   "
    ++ " |\n"
    ++ interweave s [] ys cls
interweave s (x : xs) [] cls =
  s ++ "| "
    ++ replicate (floor $ fromIntegral (fst cls - length x) / 2) ' '
    ++ x
    ++ replicate (ceiling $ fromIntegral (fst cls - length x) / 2) ' '
    ++ " | "
    ++ replicate (snd cls) ' '
    ++ " | "
    ++ "  Failed   "
    ++ " |\n"
    ++ interweave s xs [] cls
interweave s (x : xs) (y : ys) cls =
  s ++ "| "
    ++ replicate (floor $ fromIntegral (fst cls - length x) / 2) ' '
    ++ x
    ++ replicate (ceiling $ fromIntegral (fst cls - length x) / 2) ' '
    ++ " | "
    ++ replicate (floor $ fromIntegral (snd cls - length y) / 2) ' '
    ++ y
    ++ replicate (ceiling $ fromIntegral (snd cls - length y) / 2) ' '
    ++ " | "
    ++ case x == y of
      True -> "    OK     "
      False -> "  Failed   "
    ++ " |\n"
    ++ interweave s xs ys cls
interweave _ [] [] _ = ""

diffMd :: String -> String -> UtilsDiff -> String
diffMd compared1 compared2 ut = do
  let str1 = "*The options of the " ++ compared1 ++ " are not the same as those of the " ++ compared2 ++ "!*\n"
  let sos = opsSort (blockOptions1 ut) (blockOptions2 ut)
  let ub1 = fst sos
  let ub2 = snd sos

  let columnH1 = compared1 ++ " options"
  let columnH2 = compared2 ++ " options"

  let lenCol1 = length columnH1

  let lenCol2 = length columnH2

  let str2 =
        "| " ++ columnH1 ++ " | " ++ columnH2 ++ " | " ++ " Commentary |\n"
          ++ "|-"
          ++ replicate lenCol1 '-'
          ++ "-|-"
          ++ replicate lenCol2 '-'
          ++ "-|-"
          ++ replicate 11 '-'
          ++ "-|\n"

  let str3 = interweave "" ub1 ub2 (lenCol1, lenCol2)

  str1 ++ "```\n" ++ str2 ++ str3 ++ "```\n"

opsSort' :: [(String, String)] -> [(String, String)] -> ([String], [String]) -> ([String], [String])
opsSort' (x : xs) (y : ys) (r1, r2) = do
  if x == y
    then opsSort' xs ys (r1 ++ [fst x], r2 ++ [fst y])
    else do
      if snd x == "-"
        then opsSort' xs (y : ys) (r1 ++ [fst x], r2 ++ ["-"])
        else opsSort' (x : xs) ys (r1 ++ ["-"], r2 ++ [fst y])
opsSort' [] (y : ys) (r1, r2) = opsSort' [] ys (r1 ++ ["-"], r2 ++ [fst y])
opsSort' (x : xs) [] (r1, r2) = opsSort' [] xs (r1 ++ [fst x], r2 ++ ["-"])
opsSort' [] [] l = l

opsSort :: [String] -> [String] -> ([String], [String])
opsSort l1 l2 = do
  let l1' =
        map
          ( \x ->
              case find (== x) l2 of
                Nothing -> (x, "-")
                Just x -> (x, "+")
          )
          (sort l1)
  let l2' =
        map
          ( \x ->
              case find (== x) l1 of
                Nothing -> (x, "-")
                Just x -> (x, "+")
          )
          (sort l2)
  opsSort' l1' l2' ([], [])
