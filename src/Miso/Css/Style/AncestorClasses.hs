module Miso.Css.Style.AncestorClasses where

import Data.Proxy ( Proxy )
import GHC.TypeLits ( KnownSymbol )
import Miso.Css.Segment ( SubSeg(I, R, C, T), Seg, AddSubSeg )
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
  AddAncestor ::
    KnownSymbol a =>
    Proxy a -> AncestorClasses ac -> AncestorClasses (AddSubSeg (C a) ac)
  AddTagAncestor ::
    KnownSymbol a =>
    Proxy a -> AncestorClasses ac -> AncestorClasses (AddSubSeg (T a) ac)
  AddIdAncestor ::
    KnownSymbol a =>
    Proxy a -> AncestorClasses ac -> AncestorClasses (AddSubSeg (I a) ac)
