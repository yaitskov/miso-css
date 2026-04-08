-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

module Miso.Css.Style where

-- import Data.Proxy ( Proxy(..) )
-- import Data.Singletons ( Proxy(..) )
-- import Data.Singletons.TH
-- import Data.Type.Equality
-- import System.FilePath (replaceBaseName)
import Data.Tagged
import Data.Typeable
import Data.Singletons.Base.TH
import Data.Text ( Text )
import GHC.TypeLits -- ( KnownSymbol, Symbol, symbolVa )
import Miso
import Miso.Types
import Miso.Html
import Miso.Html.Element
import Prelude
import Unsafe.Coerce ( unsafeCoerce )

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

$(promote
  [d|
    applyClassToBranch :: Eq a => a -> [a] -> [a]
    applyClassToBranch _ [] = []
    applyClassToBranch c l@(h : t)
      | c == h = t
      | otherwise = l
    |])
-- type ApplyClassToBranch :: Symbol -> [Symbol] -> [Symbol]
-- type family ApplyClassToBranch c b where
--   ApplyClassToBranch _ '[] = '[]
--   ApplyClassToBranch c (c ': b') = b'
--   ApplyClassToBranch _ b = b

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
-- type ApplyClassToElem :: Symbol -> [[Symbol]] -> [[Symbol]]
-- type family ApplyClassToElem c ac where
--   ApplyClassToElem c '[] = '[]
--   ApplyClassToElem c (b ': bs) = ApplyClassToBranch c b ': ApplyClassToElem c bs
-- applyClass prenet acs and apply applyClassToElem to every eacs
-- empty remove elem
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
-- type family ApplyClass acs c eacs where
--   ApplyClass acs c [] = [acs]
--   ApplyClass acs c (e : eacs') = ApplyClassToElem c e ':

$(promote
 [d|
  appendChild :: Eq a => [[[a]]] -> [a] -> [[[a]]] -> [[[a]]]
  appendChild [] _ peacs = peacs
  appendChild ceacs [] peacs = ceacs `append` peacs
  appendChild ceacs (pclsH : pcls') peacs =
     appendChild (applyClass [] pclsH ceacs) pcls' peacs
   |])

-- div_ :: forall p c n. KnownSymbol c => CssClass p c -> N n -> N (Elem' n p c)
-- div_ css (NilN ()) = cssClassToN css
-- div_ css n@(ConsN h t) =
--   case sameSymbol (topClass css) h of
--     Just Refl -> appN t (cssClassToN css)
--     Nothing ->   unsafeCoerce $ appN n (cssClassToN css)

-- .icon-text .icon {
iconText :: CssClass '[] "icon-text"
iconText = TopCssClass (Proxy @"icon-text")
icon :: CssClass '["icon-text"] "icon"
icon = ScopedCssClass (Proxy @"icon-text") $ TopCssClass (Proxy @"icon")

-- view :: N '[]
-- view =
--   div_ iconText $  div_ icon (NilN ())

data ElementStructure = Atomic | Composite

data E (en :: Symbol) (es :: ElementStructure) (cls :: [Symbol]) (l :: [[[Symbol]]]) where
  CDataE :: MisoString -> E "CDATA" Atomic '[] '[]
  NilE :: KnownSymbol en => Proxy en -> E  en Atomic '[] '[]
  AppClsE :: forall p c en cls eacs.
    (KnownSymbol en, KnownSymbol c) =>
    OrClass p c ->
    E en Composite cls eacs ->
    E en Composite (c : cls) (ApplyClass p c eacs)
  AppendChildE ::
    E ce cs ccls ceacs ->
    E pe Composite pcls peacs ->
    E pe Composite pcls (AppendChild ceacs pcls peacs)

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
  VNode ns tg atrs children -> VNode ns tg atrs (c : children)
  o -> o

eToView :: E en es cls eacs -> View model action
eToView = \case
  CDataE txt -> vtext txt
  NilE enp -> nodeHtml (ms $ symbolVal enp) [] []
  AppClsE orCls e -> injectClass (className orCls) (eToView e)
  AppendChildE ce pe -> appChild (Tagged @Child (eToView ce)) (eToView pe)
