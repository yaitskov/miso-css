-- | Module provides a quasi quoter translating CSS classes to Haskell functions
module Miso.Css
  ( css
  , cssToDecs
  , includeCss
  ) where

import Miso.Css.Qq ( css, cssToDecs )
import Miso.Css.IncludeCss ( includeCss )
