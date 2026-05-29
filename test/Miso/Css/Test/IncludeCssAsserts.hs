{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Miso.Css.Test.IncludeCssAsserts where

import Miso.Css ( includeCss )
import Miso.Css.Test.StyleMock
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@=?) )

-- following imports just simplify output of dumped slices
-- import Miso.Css.Gen
-- import Miso.Css.Segment
-- import Miso.Css.Style
-- import Miso.Css.Style.OrClass
-- import Miso.Css.Prelude

includeCss "test/style.css"

test_include_css :: TestTree
test_include_css =
  testGroup "IncludeCss"
  [ go """<div class="foo"></div>""" $ div_ =. foo
  , go """<div class="foo"><div class="bar"></div></div>""" $
    div_ =. foo </ div_ =. bar
  , testGroup "bad"
    [ doNotTc [] [[[(JustNow, [C "foo"], [], [])]]] $ div_ =. bar
    ]
  , testCase "golden" (css @=? style)
  ]
  where
    css :: String
    css = """
      .foo > .bar {
        color: #f212ff;
      }
    """ <> "\n"
