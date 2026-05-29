{-# LANGUAGE QuasiQuotes #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Miso.Css.Test.QqDefs where

import Miso.Css (css)

[css|.foo > .bar {
  color: #1212ff;
}|]
