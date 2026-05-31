module Miso.Css.Parser where

import CssParser as CP
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Miso.Css.Prelude

type RelTag = [ (TagRelation, TagSelector) ]
type Selectors = [ RelTag ]
type SelIdxByLeafClass = M.Map Ident Selectors

indexByLeafClass :: SelIdxByLeafClass -> RelTag -> SelIdxByLeafClass
indexByLeafClass m = \case
  [] -> m
  reversedSels@((_, lastTs) : t) ->
    case mapMaybe atomicClassName lastTs.tagSubSelectors of
      [] -> indexByLeafClass m t
      clsNames -> indexByLeafClass (foldr (go $ reverse reversedSels) m clsNames) t
  where
    go rrs i = M.insertWith (<>) i [rrs]

atomicClassName :: TagSubSelector -> Maybe Ident
atomicClassName = \case
  AtomicClass x -> Just x
  _ -> Nothing

hashOnly :: TagSubSelector -> Maybe Ident
hashOnly = \case
  Hash i -> pure i
  _ -> Nothing

hashesOfTagSelector  :: TagSelector -> [ Ident ]
hashesOfTagSelector ts = mapMaybe hashOnly ts.tagSubSelectors

hashIndex :: Selectors -> S.Set Ident
hashIndex = S.fromList . concatMap (hashesOfTagSelector . snd) . concat

data CssIndex
  = CssIndex
  { byClass :: SelIdxByLeafClass
  , hashSet :: [Ident]
  }

indexFile :: CssFile -> CssIndex
indexFile cf =
  CssIndex
  { byClass = foldl' indexByLeafClass mempty $ fmap reverse sels
  , hashSet = S.toList $ hashIndex sels
  }
  where
    sels = fileToSelectors cf

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
