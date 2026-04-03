module Miso.Css.IncludeCss (includeCss) where

import AddDependentFile ( (</>), addDependentFile, getPackageRoot )
import Miso.Css.Escape ( escapeValIden )
import Miso.Css.Qq (cssToDecs)
import Data.Text.IO.Utf8 qualified as U
import Language.Haskell.TH.Syntax ( mkName, Q, Dec, runIO )
import Prelude
import System.FilePath (takeBaseName)

-- | like css quasi quoter but
-- css input is exported via constant equal to base file name
-- instead of cssAsLiteralText.
includeCss :: FilePath -> Q [Dec]
includeCss p = do
  ap <- (</> p) <$> getPackageRoot
  addDependentFile ap
  cssToDecs (mkName (escapeValIden $ takeBaseName p)) <$> runIO (U.readFile ap)
