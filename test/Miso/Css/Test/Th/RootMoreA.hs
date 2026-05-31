-- {-# OPTIONS_GHC -ddump-splices #-}
module Miso.Css.Test.Th.RootMoreA where

import Miso.Css.Test.StyleMock hiding (a, b, c, d)

[css|:root > .a {}
:root > div > .b {}
:root > body > div .c {}
:root > * > div .d {}
|]

test_t = testGroup cssAsLiteralText
  [ """<div class="a"></div>""" `go` page (div_ =. a)
  , doNotTc [] [[[(JustNow, [B], [R], [])]]] $ page (div_ </ div_ =. a)
  , """<div><div class="b"></div></div>""" `go` html_ (div_ </  div_ =. b)
  , """<div><b class="c"></b></div>""" `go` page (div_ </ b_ =. c)
  , """<div><b class="d"></b></div>""" `go` page (div_ </ b_ =. d)
  , """<div><b class="c"></b></div>""" `go` html_ (body_ (div_ </ b_ =. c))
  ]
