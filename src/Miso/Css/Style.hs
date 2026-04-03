{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Miso.Css.Style where

import Data.Proxy ( Proxy(..) )
import Data.Type.Equality ( type (:~:)(Refl) )
import GHC.TypeLits ( KnownSymbol, Symbol, sameSymbol )
import Prelude
import Unsafe.Coerce ( unsafeCoerce )

type Append :: forall a. [a] -> [a] -> [a]
type family Append xs ys where
  Append '[]    ys = ys
  Append (x:xs) ys = x : Append xs ys

type family Elem n p c where
  Elem '[]   p _  = p
  Elem (c ': n) p c  = Append n p
  Elem m     p c  = Append m p

data CssClass (p :: [Symbol]) (c :: Symbol) where
  TopCssClass :: KnownSymbol c => Proxy c -> CssClass '[] c
  ScopedCssClass :: KnownSymbol s => Proxy s -> CssClass p c -> CssClass (s : p) c

topClass :: KnownSymbol c => CssClass p c -> Proxy c
topClass (TopCssClass c) = c
topClass (ScopedCssClass _ s) = topClass s

data N (l :: [Symbol]) where
  NilN :: () -> N '[]
  ConsN :: KnownSymbol x => Proxy x -> N l -> N (x : l)

cssClassToN :: CssClass p c -> N p
cssClassToN (TopCssClass _) = NilN ()
cssClassToN (ScopedCssClass p s) = ConsN p (cssClassToN s)

appN :: N a -> N b -> N (Append a b)
appN (NilN ()) b = b
appN (ConsN x t) b = ConsN x (appN t b)

div_ :: forall p c n. KnownSymbol c => CssClass p c -> N n -> N (Elem n p c)
div_ css (NilN ()) = cssClassToN css
div_ css n@(ConsN h t) =
 case sameSymbol (topClass css) h of
   Just Refl -> appN t (cssClassToN css)
   Nothing ->   unsafeCoerce $ appN n (cssClassToN css)

-- .icon-text .icon {
iconText :: CssClass '[] "icon-text"
iconText = TopCssClass (Proxy @"icon-text")
icon :: CssClass '["icon-text"] "icon"
icon = ScopedCssClass (Proxy @"icon-text") $ TopCssClass (Proxy @"icon")

view :: N '[]
view =
  div_ iconText $  div_ icon (NilN ())
