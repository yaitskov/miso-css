{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Miso.Css.Test.QqAsserts where

import Miso.Css
import Miso.Css.Test.StyleMock
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@=?) )

[css|.foo > .bar {
  color: #1212ff;
}|]

renameCssTextConst "css2"
[css|.x {}
|]

test_qq :: TestTree
test_qq =
  testGroup "QQ"
  [ go """<div class="foo"></div>""" $ div_ =. foo
  , go """<div class="foo"><div class="bar"></div></div>""" $
    div_ =. foo </ div_ =.bar
  , testGroup "bad"
    [ doNotTc [] [[[(JustNow, [C "foo"], [], [])]]] $ div_ =. bar
    ]
  , testCase "golden" (expectedCss @=? cssAsLiteralText)
  , testCase "golden2" ((".x {}\n" :: String) @=? css2)
  ]
  where
    expectedCss :: String
    expectedCss = """
      .foo > .bar {
        color: #1212ff;
      }
    """
