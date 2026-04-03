{-# LANGUAGE TemplateHaskell #-}
module Miso.Css.Test.IncludeCssDefs where

import Miso.Css ( includeCss, CssIdentifier(id_) )

includeCss "test/style.css"

data F212ff
