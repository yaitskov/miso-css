# miso-css

## Motivation

CSS rule is applied to all nodes matching its selector.  CSS class of
an atomic selector can be applied to any DOM element, but that is not
true for classes in composite selectors, consisting from a sequence of
classes or plain tag name.

The general rule is anscetors of every node, a CSS class is applied to,
should containt all classes in the selector preceding the appiled class.

## Usage


### Quasi-quote input


``` haskell
{-# LANGUAGE QuasiQuotes #-}
module Css where
import Miso.Css ( css )

[css|
.c .b .a {
  color: #fc2c2c;
}
|]
```

``` haskell
module Main where

import Css (a, b, c, cssAsLiteralText)
import Miso
import Miso.Css.Html (toView, div_, button_, class_)

app :: App Model Action
app = (component emptyModel updateModel viewModel)
  { styles = [ Style cssAsLiteralText ] }

viewModel :: Model -> View Model Action
viewModel _ =
  toView $
    div_ [ class_ c ]
      [ div_ [ class_ b ]
        [ button_
            [ class_ a ]
            [ "Submit" ]
        ]
      ]
```

### File input
``` haskell
{-# LANGUAGE TemplateHaskell #-}
module Css where
import Miso.Css ( includeCss )

includeCss "assets/style.css"
```

``` haskell
module Main where

import Css (a, b, c, style)
-- ...
```

## Development environment

HLS should be available inside the default dev shell.

```shell
$ nix develop
$ emacs src/*/*/Qq.hs &
$ cabal build
```
