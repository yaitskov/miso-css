module Miso.Css.Test.Th.DotAdotB where

import Miso.Css.Test.StyleMock hiding (a, b)

[css|.a.b {}|]

test_t = """<div class="a b"></div>""" `go` div_ =. a =. b
