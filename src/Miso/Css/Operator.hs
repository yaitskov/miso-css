module Miso.Css.Operator where

import Data.Proxy ( Proxy )
import GHC.TypeLits ( KnownSymbol )
import Miso ( View )
import Miso.Css.List ( PrependMb, Append )
import Miso.Css.Prelude ( Maybe(Just, Nothing) )
import Miso.Css.Segment
import Miso.Css.Style
import Miso.JSON ( ToJSON )

(=.) :: (KnownSymbol en, KnownSymbol c) =>
  E model action en Composite r ei atrs kids cls eacs children ->
  OrClass p c ->
  E model action en Composite r ei ("class" : atrs) kids (c:cls)
    (ApplyClass
      (ApplySubSegsToElem
         (PrependMb
           (MbSymToMbI ei)
           (T en : A "class" : Append (SymsToAtrs atrs) (SymsToSubSeg cls)))
         p)
      (C c)
      eacs)
    children
e =. c = AppClsE c e

infixl 3 =.

atr :: forall k v. (KnownSymbol k, ToJSON v) => v -> ElAtr k v
atr = ElAtr

(=<|) :: (KnownSymbol k, ToJSON v) =>
  E model action en Composite r ei atrs kids cls eacs children ->
  ElAtr k v ->
  E model action en Composite r ei (k : atrs) kids cls
    (ApplyClass '[] (A k) eacs) -- eacs
    children
e =<| a = AddAtrE a e

infixl 3 =<|

(=#) ::
  ( KnownSymbol en
  , KnownSymbol ei
  ) =>
  E model action en Composite r Nothing catrs kids cls eacs children ->
  Proxy ei ->
  E
    model
    action
    en
    Composite
    r
    (Just ei)
    ("id" : catrs)
    (AddTagId ei kids)
    cls
    (ApplyClass '[] (A "id") (ApplyClass '[] (I ei) eacs))
    children
e =# i = IdE i e

infixl 3 =#

(</) ::
  KnownSymbol cen =>
  E model action pen Composite r       pi patrs pKids pcls peacs pchildren ->
  E model action cen cs        Nothing ci catrs cKids ccls ceacs cchildren ->
  E model action pen Composite r       pi patrs
    (MergeKids cKids pKids)
    pcls
    (ConstraintsAfterAppend pchildren ceacs pi pen patrs pcls peacs)
    (PrependMb
        (MbSymToMbI ci)
        (T cen : SymsToSubSeg ccls) : pchildren)

p </ c = AppendChildE c p

infixl 2 </

(<@) ::
  E model action pen Composite r pi patrs kids pcls peacs pchildren ->
  E model action CD Atomic Nothing Nothing '[] EmptyKids '[] '[] '[] ->
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
    ('[T CD] : pchildren)
(<@) = (</)

infixl 2 <@

(=<) ::
  E model action pen Composite r pi atrs kids pcls peacs pchildren ->
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
p =< rmv = p </ RawMisoView rmv

infixl 2 =<
