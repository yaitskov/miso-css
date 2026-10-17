{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE UndecidableInstances #-}
module Miso.Css.Operator where

import GHC.TypeLits (Nat, KnownNat)
import Miso ( View )
import Miso.Css.Event ( EventFactory )
import Miso.Css.List ( FilterOut, Append )
import Miso.Css.Prelude ( Proxy(..), Maybe(Just, Nothing), KnownSymbol, type (~) )
import Miso.Css.Segment ( SubSeg(A, T), ApplyClass )
import Miso.Css.Style
import Miso.JSON ( ToJSON )


(=.) :: (KnownSymbol en, KnownSymbol c) =>
  E model action en Composite r ei atrs kids cls eacs children cfor tabx ->
  OrClass p c ->
  E model action en Composite r ei ("class" : atrs) kids (c:cls)
    (ConstraintsAfterClassApp ei en atrs cls p c eacs)
    children cfor tabx
e =. c = AppClsE c e

infixl 3 =.

atr :: forall k v. (KnownSymbol k, ToJSON v) => v -> ElAtr k v
atr = ElAtr

(=<|) :: (KnownSymbol k, ToJSON v, RegularAtr k ~ "") =>
  E model action en Composite r ei atrs kids cls eacs children cfor tabx ->
  ElAtr k v ->
  E model action en Composite r ei (k : atrs) kids cls
    (ApplyClass '[] (A k) eacs) -- eacs
    children cfor tabx
e =<| a = AddAtrE a e

infixl 3 =<|

for :: forall model action en r ei catrs kids cls eacs children ee cfor tabx .
  (KnownSymbol en, KnownSymbol ee) =>
  E model action en Composite r ei catrs kids cls eacs children cfor tabx ->
  forall fi -> (fi ~ ElementId ee) =>
  E model action en Composite r ei catrs kids cls eacs children
  (AddForId ee cfor kids) tabx
e `for` _ = AddForAtrE (Proxy @ee) e
infixl 3 `for`

tabIndex :: forall model action en r ei catrs kids cls eacs children cfor tabx.
  (KnownSymbol en) =>
  E model action en Composite r ei catrs kids cls eacs children cfor tabx ->
  forall (ti :: Nat) -> KnownNat ti =>
  E model action en Composite r ei catrs kids cls eacs children cfor (Just ti : tabx)
e `tabIndex` _ = TabIndexE (Proxy ) e
infixl 3 `tabIndex`

tabIndexMinus1 ::
  E model action en Composite r ei atrs kids cls eacs children cfor tabx ->
  E model action en Composite r ei atrs kids cls eacs children cfor (Nothing : tabx)
tabIndexMinus1 e = TabIndexMinus1E e

ariaLabelledBy :: forall model action en r ei catrs kids cls eacs children ee cfor tabx.
  (KnownSymbol en, KnownSymbol ee) =>
  E model action en Composite r ei catrs kids cls eacs children cfor tabx ->
  forall fi -> (fi ~ ElementId ee) =>
  E model action en Composite r ei catrs kids cls eacs children
  (AddForId ee cfor kids) tabx
e `ariaLabelledBy` _ = AddAriaIdAtrE (Proxy @"aria-labelledby") (Proxy @ee) e
infixl 3 `ariaLabelledBy`

ariaDescribedBy :: forall model action en r ei catrs kids cls eacs children ee cfor tabx.
  (KnownSymbol en, KnownSymbol ee) =>
  E model action en Composite r ei catrs kids cls eacs children cfor tabx ->
  forall fi -> (fi ~ ElementId ee) =>
  E model action en Composite r ei catrs kids cls eacs children
  (AddForId ee cfor kids) tabx
e `ariaDescribedBy` _ = AddAriaIdAtrE (Proxy @"aria-describedby") (Proxy @ee) e
infixl 3 `ariaDescribedBy`

(=#) :: forall model action en r catrs kids cls eacs children ee cfor tabx.
  (KnownSymbol en, KnownSymbol ee) =>
  E model action en Composite r Nothing catrs kids cls eacs children cfor tabx ->
  forall ei -> (ei ~ ElementId ee) =>
  E model
    action
    en
    Composite
    r
    (Just ee)
    ("id" : catrs)
    (AddTagId ee kids)
    cls
    (ConstraintsAfteId ee eacs)
    children
    (FilterOut ee cfor)
    tabx
e =# _ = IdE (Proxy @ee) e

infixl 3 =#

(=!) ::
  EventFactory ef action =>
  E model action en es r ei atrs kids cls eacs children cfor tabx ->
  ef ->
  E model action en es r ei atrs kids cls eacs children cfor tabx
e =! ef = BindEventE ef e

infixl 3 =!

(</) ::
  KnownSymbol cen =>
  E model action pen Composite r       pi patrs pKids pcls peacs pchildren pfor ptabx ->
  E model action cen cs        Nothing ci catrs cKids ccls ceacs cchildren cfor ctabx ->
  E model action pen Composite r       pi patrs
    (MergeKids cKids pKids)
    pcls
    (ConstraintsAfterAppend pchildren ceacs pi pen patrs pcls peacs)
    (ChildrenConstrAfterAppend ci cen ccls pchildren)
    (MergeFors pKids cKids pfor cfor)
    (Append ptabx ctabx)
p </ c = AppendChildE c p

infixl 2 </

(<@) ::
  E model action pen Composite r pi patrs kids pcls peacs pchildren pfor tabx ->
  E model action CD Atomic Nothing Nothing '[] EmptyKids '[] '[] '[] '[] '[] ->
  E
    model
    action
    pen
    Composite
    r
    pi
    patrs
    (MergeKids EmptyKids kids)
    pcls
    (ConstraintsAfterAppend pchildren '[] pi pen patrs pcls peacs)
    (ChildrenConstrAfterAppend Nothing CD '[] pchildren)
    (MergeFors kids EmptyKids pfor '[])
    (Append tabx '[])
(<@) = (</)

infixl 2 <@

(=<) ::
  E model action pen Composite r pi atrs kids pcls peacs pchildren pfor tabx ->
  View model action ->
  E
    model
    action
    pen
    Composite
    r
    pi
    atrs
    (MergeKids EmptyKids kids)
    pcls
    (ConstraintsAfterAppend pchildren '[] pi pen atrs pcls peacs)
    ('[T RMV] : pchildren)
    (MergeFors kids EmptyKids pfor '[])
    (Append tabx '[])
p =< rmv = p </ RawMisoView rmv

infixl 2 =<
