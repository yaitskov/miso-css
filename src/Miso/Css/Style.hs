-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}

module Miso.Css.Style where

import Data.Maybe.Singletons ( JustSym0, NothingSym0 )
import Data.List.Singletons
import Data.Singletons.Base.TH
import Data.String ( IsString(..) )
import GHC.TypeLits ( KnownSymbol, Symbol )
import Miso ( MisoString, ms )
import Prelude


$(promote [d|
  append :: [a] -> [a] -> [a]
  -- does not check:
  -- append x y = foldr (:) y x
  {- HLINT ignore "Use foldr" -}
  append (x:xs) y = x : append xs y
  append [] y = y
 |])

data SubSeg
  = C Symbol -- ^ Element Class
  | T Symbol -- ^ Element Name (Tag)
  | I Symbol -- ^ Element Id
  deriving (Show, Eq, Ord)

promoteEqInstance ''SubSeg

-- | Composite segment
-- matched part is appended to unmatched when algorithm goes up to parent node
-- if unmatched is not empty
-- if unmatched is empty then list head is dropped
-- every SubSeg match step (ie class, id or tag name)
type Seg =
  ( [SubSeg] -- unmatched
  , [SubSeg] -- matched
  )

type family AddSubSeg (c :: SubSeg) (ac :: [Seg]) where
  AddSubSeg c '[] = '[ '( '[ c ] , '[] ) ]
  AddSubSeg c ( '(um, m) : t) = ( '(c : um, m) : t)

data AncestorClasses (p :: [Seg]) where
  CssOrphan :: AncestorClasses '[]
  NextAncestor ::
    AncestorClasses ac -> AncestorClasses ('( '[], '[]) : ac)
  AddAncestor ::
    KnownSymbol a =>
    Proxy a -> AncestorClasses ac -> AncestorClasses (AddSubSeg (C a) ac)
  AddTagAncestor ::
    KnownSymbol a =>
    Proxy a -> AncestorClasses ac -> AncestorClasses (AddSubSeg (T a) ac)
  AddIdAncestor ::
    KnownSymbol a =>
    Proxy a -> AncestorClasses ac -> AncestorClasses (AddSubSeg (I a) ac)

-- | 'OrClass' describes all posible selector prefixes
-- possible for the last selector segment
data OrClass
       (p :: [[Seg]]) --
       (c :: Symbol)
       -- value for tag class ie .a.b => <div class="a b">
       -- how to express that it is applicable only to tag X?
       -- another type attribute Maybe TagName
  where
    TopOrClass :: KnownSymbol c => Proxy c -> OrClass '[] c
    AddAncestorBranch :: AncestorClasses ac -> OrClass bs c -> OrClass (ac ': bs) c
$(promote
 [d|
  applySubSegToSeg :: SubSeg -> Seg -> Seg
  applySubSegToSeg _ ([], m) = ([], m)
  applySubSegToSeg ss (umH : umT, m)
   | ss == umH = (umT, umH : m)
   | otherwise = (umH : umT, m)
   |])

$(promote
  [d|
    applyClassToBranch :: SubSeg -> [Seg] -> [Seg]
    applyClassToBranch _ [] = []
    applyClassToBranch ss (h : t) = applySubSegToSeg ss h : t
    |])

$(promote
 [d|
   applyClassToElem :: SubSeg -> [[Seg]] -> [[Seg]]
   applyClassToElem _ [] = []
   applyClassToElem c (b : bs) = applyClassToBranch c b : applyClassToElem c bs
   |])

$(promote
 [d|
   applyClass :: [[Seg]] -> SubSeg -> [[[Seg]]] -> [[[Seg]]]
   applyClass [] _ [] = []
   applyClass acs _ [] = [acs]
   applyClass acs c (h : eacs) = applyClassToElem c h : applyClass acs c eacs
   |])

$(promote
 [d|
  -- [[Seg]] is element (list of branches)
  filterOutFullyMatchedHead :: [[Seg]] -> Maybe [[Seg]]
  filterOutFullyMatchedHead [] = Nothing
  -- empty branch
  filterOutFullyMatchedHead ([] : t) =
    filterOutFullyMatchedHead t
  -- matched head seg in branch
  filterOutFullyMatchedHead ((([], _matched) : firstBranchTail) : bs) =
    filterOutFullyMatchedHead (firstBranchTail : bs)
  -- reset
  filterOutFullyMatchedHead (((unMatched, matched) : firstBranchTail) : bs) =
    Just (((unMatched `append` matched, []) : firstBranchTail) : bs)
   |])

$(promote
 [d|
  mapMaybeFilterOutFullyMatchedHead :: [[[Seg]]] -> [[[Seg]]]
  mapMaybeFilterOutFullyMatchedHead [] = []
  mapMaybeFilterOutFullyMatchedHead (h:t) =
    case filterOutFullyMatchedHead h of
      Nothing -> mapMaybeFilterOutFullyMatchedHead t
      Just h' -> h' : mapMaybeFilterOutFullyMatchedHead t
   |])

-- sMapMaybe filterOutFullyMatchedHead ceacs `append` peacs
-- Not in scope: type constructor or class ‘SMapMaybeSym0’
--  • Perhaps use one of these:
--      ‘MapMaybeSym0’ (imported from Data.Maybe.Singletons),
--      ‘MapMaybeSym1’ (imported from Data.Maybe.Singletons),
--      ‘MapMaybeSym2’ (imported from Data.Maybe.Singletons) (lsp)

$(promote
 [d|
  appendChild :: [[[Seg]]] -> [SubSeg] -> [[[Seg]]] -> [[[Seg]]]
  appendChild ceacs [] peacs =
    mapMaybeFilterOutFullyMatchedHead ceacs `append` peacs
  appendChild ceacs (pclsH : pcls') peacs =
    appendChild (applyClass [] pclsH ceacs) pcls' peacs
   |])

$(promote
 [d|
  prependMb :: Eq a => Maybe a -> [a] -> [a]
  prependMb Nothing l = l
  prependMb (Just x) l = x : l
   |])

$(promote
 [d|
  findDup :: Eq a => [a] -> Maybe a
  findDup [] = Nothing
  findDup (h : t) =
    if h `elem` t
      then Just h
      else findDup t
   |])

type AppendUniq x l = x : l

type MergeUniq a b = Append a b

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
     (l :: [[[Seg]]])
  where
    CDataE ::
      MisoString ->
      E CD Atomic Nothing (UnSet '[]) '[] '[]
    NilE :: KnownSymbol en =>
      Proxy en ->
      E en Composite Nothing (UnSet '[]) '[] '[]
    IdE :: (KnownSymbol ei, FindDup (AppendUniq ei kids) ~ Nothing) =>
      Proxy ei ->
      E en Composite Nothing kids cls eacs ->
      E en Composite (Just ei) (AppendUniq ei kids) cls eacs
    AppClsE :: forall p c en cls eacs ei kIds.
      (KnownSymbol en, KnownSymbol c) =>
      OrClass p c ->
      E en Composite ei kIds cls eacs ->
      E en Composite ei kIds (c : cls) (ApplyClass p (C c) eacs)
    AppendChildE ::
      (FindDup (MergeUniq ckIds pkIds) ~ Nothing) =>
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
