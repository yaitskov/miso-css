{-
   #k .a div {
      .x .y .z, .i .j .k {
      }
   }
  ==>
  #k .a div .x .y .z { }
  #k .a div .i .j .k { }
-}

module Miso.Css.Linear where

import Data.List ( unsnoc )
import Data.Map.Strict qualified as M
import Miso.Css.Parser
    ( CssAst(rules),
      StyleRule(selectors, nested),
      Selector(root, suffix),
      SegmentRelation, SelectorSegment )
import Miso.Css.Prelude

concatSelectors :: Selector -> SegmentRelation -> Selector -> Selector
concatSelectors p r c = p { suffix = p.suffix <> ((r, c.root) : c.suffix) }

ruleSelectors :: StyleRule -> [ Selector ]
ruleSelectors sr = concatMap go sr.selectors
  where
   go parentSel =
     concatMap
       (\(segRel, childSelectors) -> fmap (concatSelectors parentSel segRel) childSelectors)
       n
   n = fmap (fmap ruleSelectors) sr.nested

linearize :: CssAst -> [ Selector ]
linearize = concatMap ruleSelectors . rules

lastSegment :: Selector -> SelectorSegment
lastSegment s =
  case unsnoc s.suffix of
    Nothing -> s.root
    Just (_, (_, sufls)) -> sufls

leafSegmentMap :: [ Selector ] -> M.Map SelectorSegment [ Selector ]
leafSegmentMap ss = foldl' go mempty $ zip (lastSegment <$> ss) ss
  where
    go m (lsOfs, s) =  M.insertWith (<>) lsOfs [s] m
