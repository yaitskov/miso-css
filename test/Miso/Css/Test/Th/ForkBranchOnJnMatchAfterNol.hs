-- {-# OPTIONS_GHC -ddump-splices #-}
module Miso.Css.Test.Th.ForkBranchOnJnMatchAfterNol where

import Miso.Css.Test.StyleMock hiding (a, b, c, d)

[css|/* ForkBranchOnJn */
.a > .b {}
.x > .y .z {}
p > b .e {}
|]

test_t = testGroup cssAsLiteralText
  [ """<div class="a"><div class="b"></div></div>"""
    `go` page (div_ =. a </ div_ =. b)
  , doNotTcNoBr [] [[[(JustNow, [B], [C "a"], [])]]]
    $ div_ =. a </ (div_ =. b </ div_ =. b)
  , """<div class="x"><div class="y"><div class="z"></div></div></div>"""
    `go` div_ =. x </ (div_ =. y </ div_ =. z)
  , """<div class="x"><div class="y"><div class="y"><div class="z"></div></div></div></div>"""
    `go` div_ =. x </ (div_ =. y </ (div_ =. y </ div_ =. z))
  , """<div class="x"><div class="y"><div class="y"><div class="y"><div class="z"></div></div></div></div></div>"""
    `go` div_ =. x </ (div_ =. y </ (div_ =. y </ (div_ =. y </ div_ =. z)))
  , """<div class="x"><div class="y"><div class="a"><div class="y"><div class="z"></div></div></div></div></div>"""
    `go` div_ =. x </ (div_ =. y </ (div_ =. a </ (div_ =. y </ div_ =. z)))
  , doNotTcNoBr []
    [[ [ (NowOrLater, [C "y"], [], [])
       , (JustNow, [C "x"], [], [])
       ]
     , [(JustNow, [B], [C "x"], [])]
     , [(JustNow, [B], [C "x"], [])]
     ]]
    $  div_ =. x </ (div_ =. a </ (div_ =. y </ (div_ =. y </ div_ =. z)))
  , doNotTcNoBr []
    [[ [ (NowOrLater, [C "y"], [], [])
       , (JustNow, [C "x"], [], [])
       ]
     , [ (JustNow, [B, C "x"], [], [])]
     , [ (JustNow, [B, C "x"], [], [])]
     , [ (JustNow, [B, C "x"], [], [])]
     ]]
    $  div_ =. y </ (div_ =. y </ (div_ =. y </ div_ =. z))
  , """<p><b><b><div class="e"></div></b></b></p>"""
    `go` p_ </ (b_ </ (b_ </ div_ =. e))
  ]
