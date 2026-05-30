module Miso.Css.Test.BindEventHandlers where

import Miso.Css.Test.StyleMock


test_t = testGroup "bind event handlers"
  [ """<div ></div>""" `go` div_ =! onClick ()
  , """<button ></button>""" `go` button_ =! onKeyPress (const ())
  , """<input  name="comment"/>""" `go`
    input_ =! onChange (const ()) =<| atr @"name" ("comment" :: MisoString)
  ]
