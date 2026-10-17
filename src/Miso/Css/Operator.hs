{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE UndecidableInstances #-}
module Miso.Css.Operator where

import Data.Char (Char)
import GHC.TypeLits (Nat, KnownNat, KnownChar)
import Miso ( View )
import Miso.Css.Event ( EventFactory )
import Miso.Css.List ( FilterOut, Append )
import Miso.Css.Prelude ( Proxy(..), Maybe(Just, Nothing), KnownSymbol, type (~) )
import Miso.Css.Segment ( SubSeg(A, T), ApplyClass )
import Miso.Css.Style
import Miso.JSON ( ToJSON )


(=.) :: (KnownSymbol en, KnownSymbol c) =>
  E ctx props model action en Composite r ei atrs kids cls eacs children cfor tabx ak ->
  OrClass p c ->
  E ctx props model action en Composite r ei ("class" : atrs) kids (c:cls)
    (ConstraintsAfterClassApp ei en atrs cls p c eacs)
    children cfor tabx ak
e =. c = AppClsE c e

infixl 3 =.

atr :: forall k v. (KnownSymbol k, ToJSON v) => v -> ElAtr k v
atr = ElAtr

(=<|) :: (KnownSymbol k, ToJSON v, RegularAtr k ~ "") =>
  E ctx props model action en Composite r ei atrs kids cls eacs children cfor tabx ak ->
  ElAtr k v ->
  E ctx props model action en Composite r ei (k : atrs) kids cls
    (ApplyClass '[] (A k) eacs) -- eacs
    children cfor tabx ak
e =<| a = AddAtrE a e

infixl 3 =<|

for :: forall ctx props model action en r ei catrs kids cls eacs children ee cfor tabx ak.
  (KnownSymbol en, KnownSymbol ee) =>
  E ctx props model action en Composite r ei catrs kids cls eacs children cfor tabx ak ->
  forall fi -> (fi ~ ElementId ee) =>
  E ctx props model action en Composite r ei catrs kids cls eacs children
  (AddForId ee cfor kids) tabx ak
e `for` _ = AddForAtrE (Proxy @ee) e
infixl 3 `for`

tabIndex :: forall ctx props model action en r ei catrs kids cls eacs children cfor tabx ak.
  (KnownSymbol en) =>
  E ctx props model action en Composite r ei catrs kids cls eacs children cfor tabx ak ->
  forall (ti :: Nat) -> KnownNat ti =>
  E ctx props model action en Composite r ei catrs kids cls eacs children cfor (Just ti : tabx) ak
e `tabIndex` _ = TabIndexE (Proxy ) e
infixl 3 `tabIndex`

accessKey :: forall ctx props model action en r ei catrs kids cls eacs children cfor tabx ak.
  (KnownSymbol en) =>
  E ctx props model action en Composite r ei catrs kids cls eacs children cfor tabx ak ->
  forall (nc :: Char) -> KnownChar nc =>
  E ctx props model action en Composite r ei catrs kids cls eacs children cfor tabx (nc : ak)
e `accessKey` _ = AccessKeyE Proxy e
infixl 3 `accessKey`

tabIndexMinus1 ::
  E ctx props model action en Composite r ei atrs kids cls eacs children cfor tabx ak ->
  E ctx props model action en Composite r ei atrs kids cls eacs children cfor (Nothing : tabx) ak
tabIndexMinus1 e = TabIndexMinus1E e

ariaLabelledBy :: forall ctx props model action en r ei catrs kids cls eacs children ee cfor tabx ak.
  (KnownSymbol en, KnownSymbol ee) =>
  E ctx props model action en Composite r ei catrs kids cls eacs children cfor tabx ak ->
  forall fi -> (fi ~ ElementId ee) =>
  E ctx props model action en Composite r ei catrs kids cls eacs children
  (AddForId ee cfor kids) tabx ak
e `ariaLabelledBy` _ = AddAriaIdAtrE (Proxy @"aria-labelledby") (Proxy @ee) e
infixl 3 `ariaLabelledBy`

ariaDescribedBy :: forall ctx props model action en r ei catrs kids cls eacs children ee cfor tabx ak.
  (KnownSymbol en, KnownSymbol ee) =>
  E ctx props model action en Composite r ei catrs kids cls eacs children cfor tabx ak ->
  forall fi -> (fi ~ ElementId ee) =>
  E ctx props model action en Composite r ei catrs kids cls eacs children
  (AddForId ee cfor kids) tabx ak
e `ariaDescribedBy` _ = AddAriaIdAtrE (Proxy @"aria-describedby") (Proxy @ee) e
infixl 3 `ariaDescribedBy`

(=#) :: forall ctx props model action en r catrs kids cls eacs children ee cfor tabx ak.
  (KnownSymbol en, KnownSymbol ee) =>
  E ctx props model action en Composite r Nothing catrs kids cls eacs children cfor tabx ak ->
  forall ei -> (ei ~ ElementId ee) =>
  E ctx props model action
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
    tabx ak
e =# _ = IdE (Proxy @ee) e

infixl 3 =#

(=!) ::
  EventFactory ef model action =>
  E ctx props model action en es r ei atrs kids cls eacs children cfor tabx ak ->
  ef ->
  E ctx props model action en es r ei atrs kids cls eacs children cfor tabx ak
e =! ef = BindEventE ef e

infixl 3 =!

(</) ::
  KnownSymbol cen =>
  E ctx props model action pen Composite r       pi patrs pKids pcls peacs pchildren pfor ptabx pak ->
  E ctx props model action cen cs        Nothing ci catrs cKids ccls ceacs cchildren cfor ctabx cak ->
  E ctx props model action pen Composite r       pi patrs
    (MergeKids cKids pKids)
    pcls
    (ConstraintsAfterAppend pchildren ceacs pi pen patrs pcls peacs)
    (ChildrenConstrAfterAppend ci cen ccls pchildren)
    (MergeFors pKids cKids pfor cfor)
    (Append ptabx ctabx)
    (Append pak cak)
p </ c = AppendChildE c p

infixl 2 </

(<@) ::
  E ctx props model action pen Composite r pi patrs kids pcls peacs pchildren pfor tabx ak ->
  E ctx props model action CD Atomic Nothing Nothing '[] EmptyKids '[] '[] '[] '[] '[] '[] ->
  E ctx props model action
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
    (Append ak '[])
(<@) = (</)

infixl 2 <@

(=<) ::
  E ctx props model action pen Composite r pi atrs kids pcls peacs pchildren pfor tabx ak ->
  View ctx props model action ->
  E ctx props model action
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
    (Append ak '[])
p =< rmv = p </ RawMisoView rmv

infixl 2 =<
