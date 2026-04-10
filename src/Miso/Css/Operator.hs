module Miso.Css.Operator where

import Data.Singletons
import Data.Singletons.Base.TH
import GHC.TypeLits ( KnownSymbol )
import Miso.Css.Style
import Miso.Css.Prelude

(=.) :: (KnownSymbol en, KnownSymbol c) =>
  E en Composite ei kids cls eacs ->
  OrClass p c ->
  E en Composite ei kids (c:cls) (ApplyClass p (C c) eacs)
e =. c = AppClsE c e

infixl 3 =.

(=#) :: (KnownSymbol en, KnownSymbol ei) =>
  E en Composite Nothing kids cls eacs ->
  Proxy ei ->
  E
    en
    Composite
    (Just ei)
    (appendUniq ei kids)
    cls
    eacs -- (ApplyClass '[] (I ei) eacs)
e =# i = IdE i e

(</) ::
  E pen Composite pi pKids pcls peacs ->
  E cen cs ci cKids ccls ceacs ->
  E pen Composite pi
    (MergeUniq cKids pKids)
    pcls
    (AppendChild
     ceacs
     (PrependMb
       (Fmap (TyCon I) pi)
       (T pen : SymsToSubSeg pcls))
     peacs)
p </ c = AppendChildE c p

infixl 2 </

(<@) ::
  E pen Composite pi kids pcls peacs ->
  E CD Atomic Nothing '[] '[] '[] ->
  E
    pen
    Composite
    pi
    kids
    pcls
    (AppendChild
     '[]
     (PrependMb
       (Fmap (TyCon I) pi)
       (T pen : SymsToSubSeg pcls))
     peacs)
(<@) = (</)

infixl 2 <@
