-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}

module Miso.Css.Style where

import Data.Tagged ( Tagged(Tagged) )
import Data.Singletons.Base.TH
import Data.String ( IsString(..) )
import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Miso
    ( ms, vtext, MisoString, Attribute(ClassList), View(VNode) )
import Miso.Html ( nodeHtml )
import Prelude

$(promote [d|
  append :: [a] -> [a] -> [a]
  -- does not check:
  -- append x y = foldr (:) y x
  {- HLINT ignore "Use foldr" -}
  append (x:xs) y = x : append xs y
  append [] y = y
 |])

type family Elem' n p c where
  Elem' '[]   p _     = p
  Elem' (c ': n) p c  = Append n p
  Elem' m     p c     = Append m p

data AncestorClasses (p :: [Symbol]) where
  CssOrphan :: AncestorClasses '[]
  AddAncestor :: KnownSymbol a => Proxy a -> AncestorClasses ac -> AncestorClasses (a ': ac)

-- | 'OrClass' describes all posible selector prefixes
-- possible for the last selector segment
data OrClass
       (p :: [[Symbol]]) --
       (c :: Symbol)
       -- value for tag class ie .a.b => <div class="a b">
       -- how to express that it is applicable only to tag X?
       -- another type attribute Maybe TagName
  where
    TopOrClass :: KnownSymbol c => Proxy c -> OrClass '[] c
    AddAncestorBranch :: AncestorClasses ac -> OrClass bs c -> OrClass (ac ': bs) c


$(promote
  [d|
    applyClassToBranch :: Eq a => a -> [a] -> [a]
    applyClassToBranch _ [] = []
    applyClassToBranch c l@(h : t)
      | c == h = t
      | otherwise = l
    |])

$(promote
 [d|
   applyClassToElem' :: Eq a => [[a]] -> a -> [[a]] -> [[a]]
   applyClassToElem' r _ [] = r
   applyClassToElem' r c (b : bs) =
     case applyClassToBranch c b of
       [] -> [] -- empty branch -> empty element
       b' -> applyClassToElem' (b' : r) c bs
   applyClassToElem :: Eq a => a -> [[a]] -> [[a]]
   applyClassToElem c bs = applyClassToElem' [] c bs
   |])

$(promote
 [d|
   applyClass :: Eq a => [[a]] -> a -> [[[a]]] -> [[[a]]]
   applyClass [] _ [] = []
   applyClass acs _ [] = [acs]
   applyClass acs c (h : eacs) =
     case applyClassToElem c h of
       [] -> applyClass acs c eacs
       h' -> h' : applyClass acs c eacs
   |])

$(promote
 [d|
  appendChild :: Eq a => [[[a]]] -> [a] -> [[[a]]] -> [[[a]]]
  appendChild [] _ peacs = peacs
  appendChild ceacs [] peacs = ceacs `append` peacs
  appendChild ceacs (pclsH : pcls') peacs =
     appendChild (applyClass [] pclsH ceacs) pcls' peacs
   |])

data ElementStructure = Atomic | Composite

type CD = "CDATA"

data E (en :: Symbol) (es :: ElementStructure) (cls :: [Symbol]) (l :: [[[Symbol]]]) where
  CDataE :: MisoString -> E CD Atomic '[] '[]
  NilE :: KnownSymbol en => Proxy en -> E en Composite '[] '[]
  AppClsE :: forall p c en cls eacs.
    (KnownSymbol en, KnownSymbol c) =>
    OrClass p c ->
    E en Composite cls eacs ->
    E en Composite (c : cls) (ApplyClass p c eacs)
  AppendChildE ::
    E ce cs ccls ceacs ->
    E pe Composite pcls peacs ->
    E pe Composite pcls (AppendChild ceacs pcls peacs)

instance IsString (E CD Atomic '[] '[]) where
  fromString = CDataE . ms

injectClass :: MisoString -> View model action -> View model action
injectClass cn = \case
  VNode ns tg atrs children -> VNode ns tg (injClass atrs) children
  o -> o
  where
    injClass = \case
      [] -> [ClassList [cn]]
      ClassList cls : atrs' -> ClassList (cn : cls) : atrs'
      o : atrs' -> o : injClass atrs'

className :: forall p c. KnownSymbol c => OrClass p c -> MisoString
className _ =
  ms $ symbolVal $ Proxy @c

data Child

appChild :: Tagged Child (View model action) -> View model action -> View model action
appChild (Tagged c) = \case
  VNode ns tg atrs children -> VNode ns tg atrs $ children <> [c]
  o -> o

eToView :: E en es cls eacs -> View model action
eToView = \case
  CDataE txt -> vtext txt
  NilE enp -> nodeHtml (ms $ symbolVal enp) [] []
  AppClsE orCls e -> injectClass (className orCls) (eToView e)
  AppendChildE ce pe -> appChild (Tagged @Child (eToView ce)) (eToView pe)

toView :: E en es cls '[] -> View model action
toView = eToView

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


div_ :: E "div" Composite '[] '[]
div_ = NilE (Proxy @"div")

p_ :: E "p" Composite '[] '[]
p_ = NilE (Proxy @"p")

b_ :: E "b" Composite '[] '[]
b_ = NilE (Proxy @"b")

i_ :: E "i" Composite '[] '[]
i_ = NilE (Proxy @"i")

th_ :: E "th" Composite '[] '[]
th_ = NilE (Proxy @"th")

td_ :: E "td" Composite '[] '[]
td_ = NilE (Proxy @"td")

table_ :: E "table" Composite '[] '[]
table_ = NilE (Proxy @"table")

tr_ :: E "tr" Composite '[] '[]
tr_ = NilE (Proxy @"tr")

h1_ :: E "h1" Composite '[] '[]
h1_ = NilE (Proxy @"h1")

img_ :: E "img" Composite '[] '[]
img_ = NilE (Proxy @"img")

br_ :: E "br" Composite '[] '[]
br_ = NilE (Proxy @"br")

hr_ :: E "hr" Composite '[] '[]
hr_ = NilE (Proxy @"hr")
