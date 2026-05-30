-- {-# OPTIONS_GHC -ddump-splices #-}
module Miso.Css.Test.Th.PmoreXplusYplusZmoreAspaceBmoreC where

import Miso.Css.Test.StyleMock hiding (a, b, c)

[css|
div > ul + p + a > li b > .p {}
p > .x + .y + .z > .a .b > .c {}
|]

test_t = testGroup cssAsLiteralText
  [ """<div><ul></ul><p></p><a><li><b><span class="p"></span></b></li></a></div>"""
    `go`
    div_
      </ ul_
      </ p_
      </ (a_ </ (li_ </ (b_ </ span_ =. p)))
  , """<p><div class="x"></div><div class="y"></div><div class="z"><div class="a"><div class="b"><div class="c"></div></div></div></div></p>"""
    `go`
    p_
      </ div_ =. x
      </ div_ =. y
      </ (div_ =. z
          </ (div_ =. a
              </ (div_ =. b
                   </ (div_ =. c))))
  ]
