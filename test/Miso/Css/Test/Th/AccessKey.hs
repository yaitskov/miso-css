module Miso.Css.Test.Th.AccessKey where

import Miso.Css.Test.StyleMock

test_t = testGroup "accesskey"
  [ """<div accesskey="a">x</div>""" `go`
    div_ `accessKey` 'a' <@ "x"
  , """<div accesskey="a"><div accesskey="b">x</div></div>""" `go`
    div_ `accessKey` 'a' </ (div_ `accessKey` 'b' <@ "x")
  , """<div accesskey="a"><div accesskey="A">x</div></div>""" `go`
    div_ `accessKey` 'a' </ (div_ `accessKey` 'A' <@ "x")
  , accessKeyDoNotTc ['a'] $ div_ `accessKey` 'a' </ (div_ `accessKey` 'a' <@ "x")
  ]
