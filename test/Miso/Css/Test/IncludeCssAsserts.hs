-- {-# OPTIONS_GHC -ddump-splices #-}
module Miso.Css.Test.IncludeCssAsserts where

import Miso.Css.Test.StyleMock

includeCss "test/style.css"

test_include_css :: TestTree
test_include_css =
  testGroup "IncludeCss"
  [ """<div class="foo"></div>""" `go` div_ =. foo
  , """<div class="foo"><div class="bar"></div></div>""" `go` div_ =. foo </ div_ =. bar
  , testGroup "bad"
    [ doNotTc [] [[[(JustNow, [C "foo"], [], [])]]] $ div_ =. bar
    ]
  , testCase "golden" (expectedCss @=? style)
  ]
  where
    expectedCss :: String
    expectedCss = """
      .foo > .bar {
        color: #f212ff;
      }
    """ <> "\n"
