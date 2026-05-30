-- {-# OPTIONS_GHC -ddump-splices #-}
module Miso.Css.Test.Th.DivPlusPtildeSpanMoreAplusB where

import Miso.Css.Test.StyleMock hiding (a, b)

[css|div + p ~ span > .a + .b {}|]

test_t = testGroup cssAsLiteralText
  [ """<div><div></div><p></p><span><div class="a"></div><div class="b"></div><div></div></span></div>"""
    `go`
    (div_
     </ div_
     </ p_
     </ (span_
          </ div_ =. a
          </ div_ =. b
          </ div_))
  , """<div><div></div><p></p><span><div class="a"></div><div class="b"></div></span></div>"""
    `go`
    (div_
     </ div_
     </ p_
     </ (span_
          </ div_ =. a
          </ div_ =. b))
  ]
