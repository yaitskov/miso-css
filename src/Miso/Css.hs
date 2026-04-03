-- | Module provides a quasi quoter translating CSS classes to Haskell functions
module Miso.Css
  ( CssIdentifier(id_)
  , CssClass
  , class_
  , css
  , cssToDecs
  , includeCss
  ) where

import Miso.Css.Qq ( CssIdentifier(id_), CssClass, class_, css, cssToDecs )
import Miso.Css.IncludeCss ( includeCss )
