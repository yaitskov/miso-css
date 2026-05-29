{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Miso.Css.Test.IncludeCssDefs where

import Miso.Css ( includeCss )
-- following imports just simplify output of dumped slices
-- import Miso.Css.Gen
-- import Miso.Css.Segment
-- import Miso.Css.Style
-- import Miso.Css.Style.OrClass
-- import Miso.Css.Prelude


includeCss "test/style.css"
