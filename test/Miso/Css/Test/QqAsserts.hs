-- {-# OPTIONS_GHC -ddump-splices #-}
module Miso.Css.Test.QqAsserts where

import Miso.Css.Test.StyleMock

[css|.foo > .bar {
  color: #1212ff;
}|]

renameCssTextConst "css2"
[css|.x {}
|]

renameCssTextConst "classlessSelectors"
[css|
body > div[x] {}
#xxx {}
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
