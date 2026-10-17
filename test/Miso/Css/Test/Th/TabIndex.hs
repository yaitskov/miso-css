module Miso.Css.Test.Th.TabIndex where

import Miso.Css.Test.StyleMock

test_t = testGroup "tabindex"
  [ """<div tabindex="-1">x</div>""" `go`
    tabIndexMinus1 (div_ <@ "x")
  , """<div tabindex="-1"><div tabindex="-1">x</div></div>""" `go`
    tabIndexMinus1 (div_ </ tabIndexMinus1 (div_ <@ "x"))
  , """<div tabindex="0">x</div>""" `go`
    div_ `tabIndex` 0 <@ "x"
  , """<div tabindex="0"><div tabindex="0">x</div></div>""" `go`
    div_ `tabIndex` 0 </ (div_ `tabIndex` 0 <@ "x")
  , """<div tabindex="1">x</div>""" `go`
    div_ `tabIndex` 1 <@ "x"
  , """<div tabindex="1"><div tabindex="2">x</div></div>""" `go`
    div_ `tabIndex` 1 </ (div_ `tabIndex` 2 <@ "x")
  , tabIdxDoNotTc [1] $ div_ `tabIndex` 1 </ (div_ `tabIndex` 1 <@ "x")
  ]
