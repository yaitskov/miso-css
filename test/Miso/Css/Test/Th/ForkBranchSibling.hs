-- {-# OPTIONS_GHC -ddump-splices #-}
module Miso.Css.Test.Th.ForkBranchSibling where

import Miso.Css.Test.StyleMock


[css|span + p ~ .t {}|]

test_t = testGroup cssAsLiteralText
  [ """<div><span></span><p></p><div class="t"></div></div>"""
    `go` div_ </ span_ </ p_  </ div_ =. t
  , """<div><span></span><p></p><p></p><div class="t"></div></div>"""
    `go` div_ </ span_ </ p_ </ p_  </ div_ =. t
  ]
