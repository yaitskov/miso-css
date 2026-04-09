module Miso.Css.Operator where

import GHC.TypeLits ( KnownSymbol )
import Miso.Css.Style

(=.) :: (KnownSymbol en, KnownSymbol c) =>
  E en Composite cls eacs ->
  OrClass p c ->
  E en Composite (c:cls) (ApplyClass p c eacs)
e =. c = AppClsE c e

infixl 3 =.

(</) ::
  E pen Composite pcls peacs ->
  E cen cs ccls ceacs ->
  E pen Composite pcls (AppendChild ceacs pcls peacs)
p </ c = AppendChildE c p

infixl 2 </

(<@) ::
  E pen Composite pcls peacs ->
  E CD Atomic '[] '[] ->
  E pen Composite pcls peacs
(<@) = (</)

infixl 2 <@
