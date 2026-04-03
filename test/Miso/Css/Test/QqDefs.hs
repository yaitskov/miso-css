{-# LANGUAGE QuasiQuotes #-}
module Miso.Css.Test.QqDefs where

import Miso.Css ( css, CssIdentifier(id_) )

[css|.foo-bar {
  color: #1212ff;
}

#foo-bar {
  color: #f212ff;
}|]

data F212ff
