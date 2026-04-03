module Miso.Css.Escape where

import Data.Char
    ( isAlpha,
      isAlphaNum,
      isLowerCase,
      isUpper,
      toLower,
      toUpper )
import Prelude

escapeValIden :: String -> String
escapeValIden s =
  case escapeIdenChar <$> hyphensToCamelCase s of
    s'@(fl:ol)
      | not (isAlpha fl) -> '_' : s'
      | isUpper fl -> toLower fl : ol
      | otherwise -> s'
    [] -> []

escapeTypeIden :: String -> String
escapeTypeIden s =
  case escapeValIden s of
    es@(fl : ol)
      | isLowerCase fl -> toUpper fl : ol
      | otherwise -> es
    [] -> []

escapeIdenChar :: Char -> Char
escapeIdenChar c
  | isAlphaNum c || c == '_' = c
  | otherwise = '_'

hyphensToCamelCase :: String -> String
hyphensToCamelCase = concatMap ucFirst . splitOn '-'

ucFirst :: String -> String
ucFirst "" = ""
ucFirst (h:t) = toUpper h : t

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = go xs []
  where
    go [] acc = [reverse acc]
    go (y : ys) acc =
      if x == y
      then reverse acc : go ys []
      else go ys (y : acc)
