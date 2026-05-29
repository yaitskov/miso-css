{-# LANGUAGE MultilineStrings #-}
module Miso.Css.Test.QqAsserts where

import Miso.Css.Test.QqDefs (foo, bar, cssAsLiteralText)
import Miso.Css.Test.StyleMock
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@=?) )


test_qq :: TestTree
test_qq =
  testGroup "QQ"
  [ go """<div class="foo"></div>""" $ div_ =. foo
  , go """<div class="foo"><div class="bar"></div></div>""" $
    div_ =. foo </ div_ =.bar
  , testGroup "bad"
    [ doNotTc [] [[[(JustNow, [C "foo"], [], [])]]] $ div_ =. bar
    ]
  , testCase "golden" (css @=? cssAsLiteralText)
  ]
  where
    css :: String
    css = """
      .foo > .bar {
        color: #1212ff;
      }
    """
