module Miso.Css.Test.Th.AmoreBmoreC where

import Miso.Css.Test.StyleMock hiding (a, b, c)

[css|.a > .b > .c {}|]

test_t =
  go """<div class="a"><div class="b"><div class="c"></div></div></div>"""
  $ div_ =. a </ (div_ =. b </ div_ =. c)
