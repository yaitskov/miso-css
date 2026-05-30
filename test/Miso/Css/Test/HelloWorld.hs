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
.c .b .a {
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
<html>
  <body>
    <div class="c">
      <div class="b">
        <button class="a">
          Submit
        </button>
      </div>
    </div>
  </body>
</html>
-}
viewModel :: Model -> View Model Action
viewModel () = toView . html_ . body_ $
  div_ =. c
  </ (div_ =. b
       </ (button_ =. a
            <@ "Submit"))
