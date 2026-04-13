{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Miso.Css.List where

import Data.Maybe.Singletons ( JustSym0, NothingSym0 )
import Data.List.Singletons ( ElemSym0 )
import Data.Singletons.Base.TH
import GHC.TypeLits ( Symbol )
import Prelude

$(promote [d|
  append :: [a] -> [a] -> [a]
  -- does not check:
  -- append x y = foldr (:) y x
  {- HLINT ignore "Use foldr" -}
  append (x:xs) y = x : append xs y
  append [] y = y
 |])

$(promote
 [d|
  removeElem :: Eq a => [a] -> a -> [a] -> Maybe (a, [a])
  removeElem _ _ [] = Nothing
  removeElem s k (h:t)
   | k == h = pure (k, append s t)
   | otherwise = removeElem (h : s) k t
   |])

$(promote
 [d|
  prependMb :: Eq a => Maybe a -> [a] -> [a]
  prependMb Nothing l = l
  prependMb (Just x) l = x : l
   |])

$(promote
 [d|
  findDup :: Eq a => [a] -> Maybe a
  findDup [] = Nothing
  findDup (h : t) =
    if h `elem` t
      then Just h
      else findDup t
   |])

type AppendUniq x l = x : l

type MergeUniq a b = Append a b

type UniqueSet = [ Symbol ]
type UnSet x = x

$(promote
 [d|
  isSubSet :: Eq a => [a] -> [a] -> Bool
  isSubSet [] _ = True
  isSubSet (h:t) l =
    case removeElem [] h l of
      Nothing -> False
      Just (_, l') -> isSubSet t l'
   |])
