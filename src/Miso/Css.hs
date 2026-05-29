-- | Module provides a quasi quoter translating CSS classes to Haskell functions
module Miso.Css
  ( css
  , cssToDecs
  , includeCss
  , renameCssTextConst
  ) where

import Miso.Css.Qq ( css, cssToDecs, renameCssTextConst )
import Miso.Css.IncludeCss ( includeCss )
