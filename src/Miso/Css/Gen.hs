module Miso.Css.Gen where

import Data.Map.Strict qualified as M

import Language.Haskell.TH.Syntax
import Miso.Css.Prelude
import Miso.Css.Parser


{-
list of selectors show be
-}
selectorsToDecs :: M.Map SelectorSegment [ Selector ] -> Q [ Dec ]
selectorsToDecs _ = pure []


{- | generate definition like:

@@
  {-# INLINE foo #-}
  foo :: IsString s => CssClass s
  foo = "foo"
@@

-}
selectorToDec :: SelectorSegment -> [Selector] -> Q [ Dec ]
selectorToDec _ _  = pure []
