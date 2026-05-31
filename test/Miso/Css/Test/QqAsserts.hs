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
  [ """<div class="foo"></div>""" `go` div_ =. foo
  , """<div class="foo"><div class="bar"></div></div>""" `go` div_ =. foo </ div_ =.bar
  , """<div id="xxx"></div>""" `go` div_ =# Xxx
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
