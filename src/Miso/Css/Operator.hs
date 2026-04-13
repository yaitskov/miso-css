module Miso.Css.Operator where

import Data.Singletons.Base.TH ( Proxy, TyCon, PFunctor(Fmap) )
import GHC.TypeLits ( KnownSymbol )
import Miso ( View )
import Miso.Css.List ( FindDup, PrependMb, AppendUniq, MergeUniq )
import Miso.Css.Segment ( SubSeg(C, I, T), ApplyClass )
import Miso.Css.Style
    ( CD,
      E(RawMisoView, AppClsE, IdE, AppendChildE),
      OrClass,
      AppendChild,
      SymsToSubSeg,
      ElementStructure(Composite, Atomic),
      RMV )

import Miso.Css.Prelude ( Maybe(Just, Nothing), type (~) )

(=.) :: (KnownSymbol en, KnownSymbol c) =>
  E model action en Composite ei kids cls eacs children ->
  OrClass p c ->
  E model action en Composite ei kids (c:cls) (ApplyClass p (C c) eacs) children
e =. c = AppClsE c e

infixl 3 =.

(=#) ::
  ( KnownSymbol en
  , KnownSymbol ei
  , FindDup (AppendUniq ei kids) ~ Nothing
  ) =>
  E model action en Composite Nothing kids cls eacs children ->
  Proxy ei ->
  E
    model
    action
    en
    Composite
    (Just ei)
    (AppendUniq ei kids)
    cls
    eacs
    children
e =# i = IdE i e

infixl 3 =#

(</) ::
  (KnownSymbol cen, FindDup (MergeUniq cKids pKids) ~ Nothing) =>
  E model action pen Composite pi pKids pcls peacs pchildren ->
  E model action cen cs ci cKids ccls ceacs cchildren ->
  E model action pen Composite pi
    (MergeUniq cKids pKids)
    pcls
    (AppendChild
     pchildren
     ceacs
     (PrependMb
       (Fmap (TyCon I) pi)
       (T pen : SymsToSubSeg pcls))
     peacs)
    (PrependMb
        (Fmap (TyCon I) ci)
        (T cen : SymsToSubSeg ccls) : pchildren)

p </ c = AppendChildE c p

infixl 2 </

(<@) ::
  (FindDup kids ~ Nothing) =>
  E model action pen Composite pi kids pcls peacs pchildren ->
  E model action CD Atomic Nothing '[] '[] '[] '[] ->
  E
    model
    action
    pen
    Composite
    pi
    kids
    pcls
    (AppendChild
     pchildren
     '[]
     (PrependMb
       (Fmap (TyCon I) pi)
       (T pen : SymsToSubSeg pcls))
     peacs)
    ('[T CD] : pchildren)
(<@) = (</)

infixl 2 <@

(=<) ::
  (FindDup kids ~ Nothing) =>
  E model action pen Composite pi kids pcls peacs pchildren ->
  View model action ->
  E
    model
    action
    pen
    Composite
    pi
    kids
    pcls
    (AppendChild
     pchildren
     '[]
     (PrependMb
       (Fmap (TyCon I) pi)
       (T pen : SymsToSubSeg pcls))
     peacs)
    ('[T RMV] : pchildren)
p =< rmv = p </ RawMisoView rmv

infixl 2 =<
