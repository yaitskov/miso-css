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
  E model action en Composite r ei atrs kids cls eacs children for ->
  OrClass p c ->
  E model action en Composite r ei ("class" : atrs) kids (c:cls)
    (ConstraintsAfterClassApp ei en atrs cls p c eacs)
    children for
e =. c = AppClsE c e

infixl 3 =.

atr :: forall k v. (KnownSymbol k, ToJSON v) => v -> ElAtr k v
atr = ElAtr

(=<|) :: (KnownSymbol k, ToJSON v) =>
  E model action en Composite r ei atrs kids cls eacs children for ->
  ElAtr k v ->
  E model action en Composite r ei (k : atrs) kids cls
    (ApplyClass '[] (A k) eacs) -- eacs
    children for
e =<| a = AddAtrE a e

infixl 3 =<|

(=#) :: forall model action en r catrs kids cls eacs children ee for.
  (KnownSymbol en, KnownSymbol ee) =>
  E model action en Composite r Nothing catrs kids cls eacs children for ->
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
    (FilterOut ee for)
e =# _ = IdE (Proxy @ee) e

infixl 3 =#

(=!) ::
  EventFactory ef action =>
  E model action en es r ei atrs kids cls eacs children for ->
  ef ->
  E model action en es r ei atrs kids cls eacs children for
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
  E model action pen Composite r pi atrs kids pcls peacs pchildren for ->
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
    (MergeFors kids EmptyKids for '[])
p =< rmv = p </ RawMisoView rmv

infixl 2 =<
