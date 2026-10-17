module Miso.Css.Test.Th.For where

import Miso.Css.Test.StyleMock

[css|
#xx {}
|]

test_t = testGroup cssAsLiteralText
  [ """<div><label for="a">l</label><input id="a"/></div>""" `go`
    div_ </ ((label_ `for` Ai) <@ "l") </ input_ =# Ai
  , """<div><label for="xx">l</label><input id="xx"/></div>""" `go`
    div_ </ (label_ `for` Xx <@ "l") </ input_ =# Xx
  , """<label for="xx">l<input id="xx"/></label>""" `go`
    label_ `for` Xx <@ "l" </ input_ =# Xx
  , ariaDoNotTc ["xx"] $ label_ `for` Xx <@ "l" </ input_ =# Ai
  , """<button id="xx"><label for="xx">l</label></button>""" `go`
    button_ =# Xx </ (label_ `for` Xx <@ "l")
  , ariaDoNotTc ["xx"] $ button_ =# Ai </ (label_ `for` Xx <@ "l")
  , ariaDoNotTc ["xx"] $ div_ </ (label_ `for` Xx <@ "l") </ input_
  ]
