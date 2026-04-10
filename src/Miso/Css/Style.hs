-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}

module Miso.Css.Style where

import Data.Function.Singletons
import Data.List.Singletons
import Data.Singletons
import Data.Singletons.Base.TH
import Data.String ( IsString(..) )
-- import Data.Type.Set qualified as TS -- hiding (Proxy)
import GHC.TypeLits ( KnownSymbol, Symbol )
import Miso ( MisoString, ms )


import Prelude
import Text.Show.Singletons

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

data SubSeg
  = C Symbol -- ^ Element Class
  | T Symbol -- ^ Element Name (Tag)
  | I Symbol -- ^ Element Id
  deriving (Show, Eq, Ord)
promoteEqInstance ''SubSeg

data AncestorClasses (p :: [SubSeg]) where
  CssOrphan :: AncestorClasses '[]
  AddAncestor :: KnownSymbol a => Proxy a -> AncestorClasses ac -> AncestorClasses (C a ': ac)
  AddTagAncestor :: KnownSymbol a => Proxy a -> AncestorClasses ac -> AncestorClasses (T a ': ac)
  AddIdAncestor :: KnownSymbol a => Proxy a -> AncestorClasses ac -> AncestorClasses (I a ': ac)

-- | 'OrClass' describes all posible selector prefixes
-- possible for the last selector segment
data OrClass
       (p :: [[SubSeg]]) --
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
  appendChild ceacs [] peacs = ceacs `append` peacs
  appendChild ceacs (pclsH : pcls') peacs =
     appendChild (applyClass [] pclsH ceacs) pcls' peacs
  appendChild [] _ peacs = peacs
   |])

$(promote
 [d|
  prependMb :: Eq a => Maybe a -> [a] -> [a]
  prependMb Nothing l = l
  prependMb (Just x) l = x : l
   |])

$(promote
 [d|
  appendUniq :: (Show a, Eq a) => a -> [a] -> [a]
  appendUniq x [] = [x]
  appendUniq x (h:t)
   | x == h = error ("Duplcated on unique list: "  <> show_ x )
   | otherwise = h : appendUniq x t
   |])

$(promote
 [d|
  mergeUniq :: (Show a, Eq a) => [a] -> [a] -> [a]
  mergeUniq a l = foldl (flip appendUniq) l a
   |])

-- promoting not working
type family SymsToSubSeg l where
  SymsToSubSeg '[] = '[]
  SymsToSubSeg (h : l) = C h : SymsToSubSeg l

data ElementStructure = Atomic | Composite

type CD = "CDATA"

type UniqueSet = [ Symbol ]
type UnSet x = x

data E
     (en :: Symbol)
     (es :: ElementStructure)
     (ei :: Maybe Symbol)
     (knownIds :: UniqueSet)
     (cls :: [Symbol])
     (l :: [[[SubSeg]]])
  where
    CDataE :: MisoString -> E CD Atomic Nothing (UnSet '[]) '[] '[]
    NilE :: KnownSymbol en => Proxy en -> E en Composite Nothing (UnSet '[]) '[] '[]
    IdE :: KnownSymbol ei =>
      Proxy ei ->
      E en Composite Nothing kids cls eacs ->
      E en Composite (Just ei) (appendUniq ei kids) cls eacs
    AppClsE :: forall p c en cls eacs ei kIds.
      (KnownSymbol en, KnownSymbol c) =>
      OrClass p c ->
      E en Composite ei kIds cls eacs ->
      E en Composite ei kIds (c : cls) (ApplyClass p (C c) eacs)
    AppendChildE ::
      E ce cs ci ckIds ccls ceacs ->
      E pe Composite pi pkIds pcls peacs ->
      E pe Composite pi
      (MergeUniq ckIds pkIds)
      pcls
      (AppendChild
       ceacs
       (PrependMb
        (Fmap (TyCon I) pi)
        (T pe : SymsToSubSeg pcls))
       peacs)

instance IsString (E CD Atomic Nothing (UnSet '[]) '[] '[]) where
  fromString = CDataE . ms
