module Miso.Css.Test.Th.Aria where

import Miso.Css.Test.StyleMock

[css|
#aa {}
#bb {}
|]

test_t = testGroup cssAsLiteralText
  [ """<div><span id="aa">hello</span><input aria-labelledby="aa"/></div>""" `go`
    div_ </ (span_ =# Aa <@ "hello") </ input_ `ariaLabelledBy` Aa
  , ariaDoNotTc ["aa"] $
    div_ </ (span_ <@ "hello") </ input_ `ariaLabelledBy` Aa
  , """<div><span id="aa">hello</span><input aria-describedby="aa"/></div>""" `go`
    div_ </ (span_ =# Aa <@ "hello") </ input_ `ariaDescribedBy` Aa
  , ariaDoNotTc ["aa"] $
    div_ </ (span_ <@ "hello") </ input_ `ariaDescribedBy` Aa
  , """<div><span id="aa">hi</span><span id="bb">bye</span><input aria-describedby="aa bb"/></div>"""
    `go`
    div_ </ (span_ =# Aa <@ "hi") </ (span_ =# Bb <@ "bye")
      </ input_ `ariaDescribedBy` Aa `ariaDescribedBy` Bb
  , ariaDoNotTc ["bb"] $
    div_ </ (span_ =# Aa <@ "hi") </ input_ `ariaDescribedBy` Aa `ariaDescribedBy` Bb
  , ariaDoNotTc ["aa"] $
    div_ </ (span_ =# Bb <@ "bye") </ input_ `ariaDescribedBy` Aa `ariaDescribedBy` Bb
  ]
