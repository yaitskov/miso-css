module Miso.Css.Parser where

import Data.CSS.Syntax.Tokens
import Data.Text
import Miso.Css.Prelude

{-
consume a stream of CSS tokens.

reconstruct series of selector.
selectors from sequences of classes, tag names, ids

so need a struct to represent the info above
-}

newtype ClassName = ClassName Text deriving newtype (Show, Eq, Ord)
newtype IdKey = IdKey Text deriving newtype (Show, Eq, Ord)
newtype TagName = TagName Text deriving newtype (Show, Eq, Ord)

data SelectorSegment
  = CssClassSegment [ClassName]
  | TagSegment TagName
  | IdSegment IdKey
  | PseudoRoot
  deriving (Show, Eq, Ord)

data SegmentRelation = Descendant | Child deriving (Show, Eq)

data Selector
  = Selector
  { root :: SelectorSegment
  , suffix :: [ (SegmentRelation, SelectorSegment) ]
  } deriving (Show, Eq)

data StyleRule
  = StyleRule
  { selectors :: [ Selector ]
  , nested :: [ (SegmentRelation, StyleRule) ]
  } deriving (Show, Eq)

data CssAst = CssAst
  { rules :: [ StyleRule ]
  , originFile :: FilePath
  }
