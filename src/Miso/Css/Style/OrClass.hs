module Miso.Css.Style.OrClass where

import Data.Proxy ( Proxy )
import GHC.TypeLits ( KnownSymbol, Symbol )
import Miso.Css.Segment ( Seg )
import Miso.Css.Style.AncestorClasses ( AncestorClasses )

-- | 'OrClass' describes all posible selector prefixes
-- possible for the last selector segment
data OrClass
       (p :: [[Seg]])
       (c :: Symbol)
  where
    TopOrClass :: KnownSymbol c => Proxy c -> OrClass '[] c
    AddAncestorBranch :: AncestorClasses ac -> OrClass bs c -> OrClass (ac ': bs) c
