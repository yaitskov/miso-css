module Miso.Css.Operator where

import Data.Proxy ( Proxy )
import GHC.TypeLits ( KnownSymbol )
import Miso ( View )
import Miso.Css.List ( FindDup, PrependMb, AppendUniq, MergeUniq )
import Miso.Css.Segment
import Miso.Css.Style
    ( CD,
      E(RawMisoView, AppClsE, IdE, AppendChildE),
      OrClass,
      AppendChild,
      SymsToSubSeg,
      ElementStructure(Composite, Atomic),
      RMV )
import Miso.Css.Style.PreAppend qualified as Pre
import Miso.Css.Prelude ( Maybe(Just, Nothing), type (~) )

(=.) :: (KnownSymbol en, KnownSymbol c) =>
  E model action en Composite r ei kids cls eacs children ->
  OrClass p c ->
  E model action en Composite r ei kids (c:cls)
    (ApplyClass
      (ApplySubSegsToElem
         (PrependMb
           (MbSymToMbI ei)
           (T en : SymsToSubSeg cls))
         p)
      (C c)
      eacs)
    children
e =. c = AppClsE c e

infixl 3 =.

(=#) ::
  ( KnownSymbol en
  , KnownSymbol ei
  , FindDup (AppendUniq ei kids) ~ Nothing
  ) =>
  E model action en Composite r Nothing kids cls eacs children ->
  Proxy ei ->
  E
    model
    action
    en
    Composite
    r
    (Just ei)
    (AppendUniq ei kids)
    cls
    (ApplyClass '[] (I ei) eacs)
    children
e =# i = IdE i e

infixl 3 =#

(</) ::
  (KnownSymbol cen, FindDup (MergeUniq cKids pKids) ~ Nothing) =>
  E model action pen Composite r       pi pKids pcls peacs pchildren ->
  E model action cen cs        Nothing ci cKids ccls ceacs cchildren ->
  E model action pen Composite r       pi
    (MergeUniq cKids pKids)
    pcls
    (AppendChild
     pchildren
     (Pre.MapMaybeFilterOutFullyMatchedHead '[] ceacs)
     (PrependMb
       (MbSymToMbI pi)
       (T pen : SymsToSubSeg pcls))
     peacs)
    (PrependMb
        (MbSymToMbI ci)
        (T cen : SymsToSubSeg ccls) : pchildren)

p </ c = AppendChildE c p

infixl 2 </

(<@) ::
  (FindDup kids ~ Nothing) =>
  E model action pen Composite r pi kids pcls peacs pchildren ->
  E model action CD Atomic Nothing Nothing '[] '[] '[] '[] ->
  E
    model
    action
    pen
    Composite
    r
    pi
    kids
    pcls
    (AppendChild
     pchildren
     '[]
     (PrependMb
       (MbSymToMbI pi)
       (T pen : SymsToSubSeg pcls))
     peacs)
    ('[T CD] : pchildren)
(<@) = (</)

infixl 2 <@

(=<) ::
  (FindDup kids ~ Nothing) =>
  E model action pen Composite r pi kids pcls peacs pchildren ->
  View model action ->
  E
    model
    action
    pen
    Composite
    r
    pi
    kids
    pcls
    (AppendChild
     pchildren
     '[]
     (PrependMb
       (MbSymToMbI pi)
       (T pen : SymsToSubSeg pcls))
     peacs)
    ('[T RMV] : pchildren)
p =< rmv = p </ RawMisoView rmv

infixl 2 =<
