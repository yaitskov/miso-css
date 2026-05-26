module Miso.Css.Style.AncestorClasses where

import Data.Proxy ( Proxy )
import GHC.TypeLits ( KnownSymbol )
import Miso.Css.Segment ( SubSeg(R), Seg, AddSubSeg )
import Miso.Css.Sibling ( AddSiblingBr, SiblingBranch )


data AncestorClasses (p :: [Seg]) where
  CssOrphan :: Proxy ms -> AncestorClasses '[ '( ms, '[], '[], '[] )]
  AddRoot ::
    AncestorClasses ac -> AncestorClasses (AddSubSeg R ac)
  AddSiblingBranch ::
    SiblingBranch sgs ->
    AncestorClasses ac ->
    AncestorClasses (AddSiblingBr sgs ac)
  NextAncestor :: -- CSS star
    Proxy ms ->
    AncestorClasses ac ->
    AncestorClasses ('( ms, '[], '[], '[]) : ac)
  AddSubSegConstraint ::
    KnownSymbol a =>
    Proxy c -> Proxy a -> AncestorClasses ac -> AncestorClasses (AddSubSeg (c a) ac)
