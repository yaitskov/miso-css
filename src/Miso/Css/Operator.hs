{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE UndecidableInstances #-}
module Miso.Css.Operator where

import Miso ( View )
import Miso.Css.Event ( EventFactory )
import Miso.Css.List ( FilterOut )
import Miso.Css.Prelude ( Proxy(..), Maybe(Just, Nothing), KnownSymbol, type (~) )
import Miso.Css.Segment ( SubSeg(A, T), ApplyClass )
import Miso.Css.Style
import Miso.JSON ( ToJSON )


(=.) :: (KnownSymbol en, KnownSymbol c) =>
  E model action en Composite r ei atrs kids cls eacs children cfor ->
  OrClass p c ->
  E model action en Composite r ei ("class" : atrs) kids (c:cls)
    (ConstraintsAfterClassApp ei en atrs cls p c eacs)
    children cfor
e =. c = AppClsE c e

infixl 3 =.

atr :: forall k v. (KnownSymbol k, ToJSON v) => v -> ElAtr k v
atr = ElAtr

(=<|) :: (KnownSymbol k, ToJSON v, RegularAtr k ~ "") =>
  E model action en Composite r ei atrs kids cls eacs children cfor ->
  ElAtr k v ->
  E model action en Composite r ei (k : atrs) kids cls
    (ApplyClass '[] (A k) eacs) -- eacs
    children cfor
e =<| a = AddAtrE a e

infixl 3 =<|

for :: forall model action en r ei catrs kids cls eacs children ee cfor.
  (KnownSymbol en, KnownSymbol ee) =>
  E model action en Composite r ei catrs kids cls eacs children cfor ->
  forall fi -> (fi ~ ElementId ee) =>
  E model action en Composite r ei catrs kids cls eacs children
  (AddForId ee cfor kids)
e `for` _ = AddForAtrE (Proxy @ee) e
infixl 3 `for`

ariaLabelledBy :: forall model action en r ei catrs kids cls eacs children ee cfor.
  (KnownSymbol en, KnownSymbol ee) =>
  E model action en Composite r ei catrs kids cls eacs children cfor ->
  forall fi -> (fi ~ ElementId ee) =>
  E model action en Composite r ei catrs kids cls eacs children
  (AddForId ee cfor kids)
e `ariaLabelledBy` _ = AddAriaIdAtrE (Proxy @"aria-labelledby") (Proxy @ee) e
infixl 3 `ariaLabelledBy`

ariaDescribedBy :: forall model action en r ei catrs kids cls eacs children ee cfor.
  (KnownSymbol en, KnownSymbol ee) =>
  E model action en Composite r ei catrs kids cls eacs children cfor ->
  forall fi -> (fi ~ ElementId ee) =>
  E model action en Composite r ei catrs kids cls eacs children
  (AddForId ee cfor kids)
e `ariaDescribedBy` _ = AddAriaIdAtrE (Proxy @"aria-describedby") (Proxy @ee) e
infixl 3 `ariaDescribedBy`

(=#) :: forall model action en r catrs kids cls eacs children ee cfor.
  (KnownSymbol en, KnownSymbol ee) =>
  E model action en Composite r Nothing catrs kids cls eacs children cfor ->
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
e =# _ = IdE (Proxy @ee) e

infixl 3 =#

(=!) ::
  EventFactory ef action =>
  E model action en es r ei atrs kids cls eacs children cfor ->
  ef ->
  E model action en es r ei atrs kids cls eacs children cfor
e =! ef = BindEventE ef e

infixl 3 =!

(</) ::
  KnownSymbol cen =>
  E model action pen Composite r       pi patrs pKids pcls peacs pchildren pfor ->
  E model action cen cs        Nothing ci catrs cKids ccls ceacs cchildren cfor ->
  E model action pen Composite r       pi patrs
    (MergeKids cKids pKids)
    pcls
    (ConstraintsAfterAppend pchildren ceacs pi pen patrs pcls peacs)
    (ChildrenConstrAfterAppend ci cen ccls pchildren)
    (MergeFors pKids cKids pfor cfor)

p </ c = AppendChildE c p

infixl 2 </

(<@) ::
  E model action pen Composite r pi patrs kids pcls peacs pchildren pfor ->
  E model action CD Atomic Nothing Nothing '[] EmptyKids '[] '[] '[] '[] ->
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
(<@) = (</)

infixl 2 <@

(=<) ::
  E model action pen Composite r pi atrs kids pcls peacs pchildren pfor ->
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
p =< rmv = p </ RawMisoView rmv

infixl 2 =<
