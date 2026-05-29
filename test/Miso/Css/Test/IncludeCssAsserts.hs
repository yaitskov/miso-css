{-# LANGUAGE MultilineStrings #-}
module Miso.Css.Test.IncludeCssAsserts where

import Miso.Css.Test.IncludeCssDefs (foo, bar, style)
import Miso.Css.Test.StyleMock
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@=?) )

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
