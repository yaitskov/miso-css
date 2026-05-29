module Miso.Css.Test.Th.AplusBplusC where

import Miso.Css.Test.StyleMock hiding (a, b, c)

[css|.a + .b + .c {} |]

test_t =
  go """<div><div class="a"></div><div class="b"></div><div class="c"></div></div>"""
  $ div_ </ div_ =. a </ div_ =. b </ div_ =. c
