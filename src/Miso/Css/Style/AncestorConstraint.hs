module Miso.Css.Style.AncestorConstraint where

import Data.Proxy ( Proxy )
import GHC.TypeLits ( KnownSymbol )
import Miso.Css.Segment ( SubSeg(R), Seg, AddSubSeg )
import Miso.Css.Sibling ( AddSiblingBr, SiblingBranch )


data AncestorConstraint (p :: [Seg]) where
  CssOrphan :: Proxy ms -> AncestorConstraint '[ '( ms, '[], '[], '[] )]
  AddRoot ::
    AncestorConstraint ac -> AncestorConstraint (AddSubSeg R ac)
  AddSiblingBranch ::
    SiblingBranch sgs ->
    AncestorConstraint ac ->
    AncestorConstraint (AddSiblingBr sgs ac)
  NextAncestor :: -- CSS star
    Proxy ms ->
    AncestorConstraint ac ->
    AncestorConstraint ('( ms, '[], '[], '[]) : ac)
  AddSubSegConstraint ::
    KnownSymbol a =>
    Proxy c -> Proxy a -> AncestorConstraint ac -> AncestorConstraint (AddSubSeg (c a) ac)
