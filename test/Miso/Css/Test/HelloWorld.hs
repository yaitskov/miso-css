{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Miso.Css.Test.HelloWorld where

import Miso ( component, App, CSS(Style), Component(styles), View )
import Miso.Css
import Prelude

type Model = ()
type Action = ()

-- default name is "cssAsLiteralText"
renameCssTextConst "cssFromQq"

[css|
.a .b .c {
  color: #fc2c2c;
}
|]

-- instead of quasi-quoted CSS
-- the whole CSS file can be included with:
--   includeCss "assets/style.css"

app :: App Model Action
app = (component () pure viewModel)
  { styles = [ Style cssFromQq ] }

{-
    <div class="a">
      <div class="b">
        <button class="b">
          Submit
        </button>
      </div>
    </div>
-}
viewModel :: Model -> View Model Action
viewModel () = toView . html_ . body_ $
  div_ =. a
  </ (div_ =. b
       </ (button_ =. c
            <@ "Submit"))
