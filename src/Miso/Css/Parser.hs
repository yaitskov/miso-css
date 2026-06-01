module Miso.Css.Parser where

import CssParser as CP
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Miso.Css.Prelude

type RelTag = [ (TagRelation, TagSelector) ]
type Selectors = [ RelTag ]
type SelIdxByLeafClass = M.Map Ident Selectors

data RulesForNonLeafClasses = RulesForNonLeafClasses deriving (Show, Eq, Ord)

type ParserM = Reader (Maybe RulesForNonLeafClasses)

indexByLeafClass :: SelIdxByLeafClass -> RelTag -> ParserM SelIdxByLeafClass
indexByLeafClass m = \case
  [] -> pure m
  reversedSels@((_, lastTs) : t) ->
    case mapMaybe atomicClassName lastTs.tagSubSelectors of
      [] -> indexByLeafClass m t
      clsNames ->
        ask >>= \case
          Just RulesForNonLeafClasses ->
            indexByLeafClass (foldr (go $ reverse reversedSels) m clsNames) t
          Nothing ->
            pure $ addNonLeafClasses (foldr (go $ reverse reversedSels) m clsNames) t
  where
    go rrs i = M.insertWith (<>) i [rrs]
    addNonLeafClasses m' = \case
      [] -> m'
      (_, ts) : t ->
        addNonLeafClasses
          (foldl' (\m'' clsName -> M.insertWith (<>) clsName [] m'')
                  m'
                  (mapMaybe atomicClassName ts.tagSubSelectors))
          t

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

indexFile :: CssFile -> ParserM CssIndex
indexFile cf =
  (`CssIndex` S.toList (hashIndex sels)) <$>
    foldM indexByLeafClass mempty (fmap reverse sels)
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
