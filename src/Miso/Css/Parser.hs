module Miso.Css.Parser where

import CssParser as CP
import Miso.Css.Prelude
import Data.Map.Strict qualified as M

type RelTag = [ (TagRelation, TagSelector) ]
type Selectors = [ RelTag ]
type SelIdxByLeafClass = M.Map Ident Selectors

indexByLeafClass :: SelIdxByLeafClass -> RelTag -> SelIdxByLeafClass
indexByLeafClass m = \case
  [] -> m
  rs@((_, ts) : t) ->
    case mapMaybe atomicClassName ts.tagSubSelectors of
      [] -> indexByLeafClass m t
      cns -> indexByLeafClass (foldl' (go rs) m cns) t
  where
    go rs b i = M.insertWith (<>) i [rs] b

atomicClassName :: TagSubSelector -> Maybe Ident
atomicClassName = \case
  AtomicClass x -> Just x
  _ -> Nothing

indexFile :: CssFile -> SelIdxByLeafClass
indexFile = foldl' indexByLeafClass mempty . fileToSelectors

fileToSelectors :: CssFile -> Selectors
fileToSelectors cf = concatMap ruleToSelectors cf.rules

ruleToSelectors :: CssRule -> Selectors
ruleToSelectors =
  fmap (concatMap selectorToTagRelSel) . extractSelectors

selectorToTagRelSel :: Selector -> RelTag
selectorToTagRelSel = \case
  Selector ftr fts os -> (fromMaybe Descendant ftr, fts) : os
  PeSelector ftr fts os _ -> (fromMaybe Descendant ftr, fts) : os
  PeSelectorOnly {} -> []


extractSelectors :: CssRule -> [[Selector]]
extractSelectors = \case
  CssRule sels subRules ->
    let selsList = toList sels in
    (pure <$> selsList) <> [ a : b |  a <- selsList, b <- concatMap goSubRules subRules ]
  AtRule {} -> []
  where
    goSubRules = \case
      CssLeafRule {} -> []
      CssEnumLeaf {} -> []
      CssNestedRule cr -> extractSelectors cr
