-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}

module Miso.Css.Style where

import Data.List.Singletons
import Data.Singletons.Base.TH
import Data.String ( IsString(..) )
import GHC.TypeLits ( KnownSymbol, Symbol )
import Miso ( MisoString, ms )
import Miso.Css.List
import Miso.Css.Segment
import Miso.Css.Sibling
import Prelude

data AncestorClasses (p :: [Seg]) where
  CssOrphan :: Proxy ms -> AncestorClasses '[ '( ms, '[], '[], '[] )]
  AddSiblingBranch ::
    SiblingBranch sgs ->
    AncestorClasses ac ->
    AncestorClasses (AddSiblingBr sgs ac)
  NextAncestor :: -- CSS star
    Proxy ms ->
    AncestorClasses ac ->
    AncestorClasses ('( ms, '[], '[], '[]) : ac)
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
  -- [[Seg]] is element (list of branches)
  filterOutFullyMatchedHead :: [[SubSeg]] -> [[Seg]] -> [[Seg]] -> [[Seg]]
  filterOutFullyMatchedHead _siblings r [] = r
  -- empty branch
  filterOutFullyMatchedHead _siblings _r ([] : _t) = [] -- filterOutFullyMatchedHead t
  -- matched last branch segment
  filterOutFullyMatchedHead _siblings _r ( [ (_mts, [], _matched, []) ] : _bs) = []
  -- matched head seg in branch
  filterOutFullyMatchedHead siblings r (((_mts, [], _matched, []) : firstBranchTail) : bs) =
    filterOutFullyMatchedHead siblings (firstBranchTail:r) bs
  -- filter siblings after all
  filterOutFullyMatchedHead siblings r (((_mts, [], _matched, siblingBranches) : firstBranchTail) : bs) =
    case matchSiblings [] siblings siblingBranches of
      [] -> []
      siblingBranches' ->
        filterOutFullyMatchedHead
          siblings
          (((_mts, [B], _matched, siblingBranches') : firstBranchTail) : r)
          bs
  -- skip locked
  filterOutFullyMatchedHead siblings r (((_mts, B : unMatched, matched, sib) : firstBranchTail) : bs) =
    filterOutFullyMatchedHead siblings (((_mts, B : unMatched, matched, sib) : firstBranchTail) : r) bs

  -- reset
  filterOutFullyMatchedHead siblings r (((NowOrLater, unMatched, matched, sib) : firstBranchTail) : bs) =
    filterOutFullyMatchedHead siblings (((NowOrLater, unMatched `append` matched, [], sib) : firstBranchTail) : r) bs

  -- lock
  filterOutFullyMatchedHead siblings r (((JustNow, unMatched, matched, sib) : firstBranchTail) : bs) =
    filterOutFullyMatchedHead siblings (((JustNow, B : unMatched, matched, sib) : firstBranchTail) : r) bs
   |])

$(promote
 [d|
  mapMaybeFilterOutFullyMatchedHead :: [[SubSeg]] -> [[[Seg]]] -> [[[Seg]]]
  mapMaybeFilterOutFullyMatchedHead _ [] = []
  mapMaybeFilterOutFullyMatchedHead children (h:t) =
    case filterOutFullyMatchedHead children [] h of
      [] -> mapMaybeFilterOutFullyMatchedHead children t
      h' -> h' : mapMaybeFilterOutFullyMatchedHead children t
   |])

-- sMapMaybe filterOutFullyMatchedHead ceacs `append` peacs
-- Not in scope: type constructor or class ‘SMapMaybeSym0’
--  • Perhaps use one of these:
--      ‘MapMaybeSym0’ (imported from Data.Maybe.Singletons),
--      ‘MapMaybeSym1’ (imported from Data.Maybe.Singletons),
--      ‘MapMaybeSym2’ (imported from Data.Maybe.Singletons) (lsp)

$(promote
 [d|
  appendChild :: [[SubSeg]] -> [[[Seg]]] -> [SubSeg] -> [[[Seg]]] -> [[[Seg]]]
  appendChild children ceacs [] peacs =
    mapMaybeFilterOutFullyMatchedHead children ceacs `append` peacs
  appendChild children ceacs (pclsH : pcls') peacs =
    appendChild children (applyClass [] pclsH ceacs) pcls' peacs
   |])

-- promoting not working
type family SymsToSubSeg l where
  SymsToSubSeg '[] = '[]
  SymsToSubSeg (h : l) = C h : SymsToSubSeg l

data ElementStructure = Atomic | Composite

type CD = "CDATA"

data E
     (en :: Symbol)
     (es :: ElementStructure)
     (ei :: Maybe Symbol)
     (knownIds :: UniqueSet)
     (cls :: [Symbol])
     (l :: [[[Seg]]])
     (children :: [[SubSeg]])
  where
    CDataE ::
      MisoString ->
      E CD Atomic Nothing (UnSet '[]) '[] '[] '[]
    NilE :: KnownSymbol en =>
      Proxy en ->
      E en Composite Nothing (UnSet '[]) '[] '[] '[]
    IdE :: (KnownSymbol ei, FindDup (AppendUniq ei kids) ~ Nothing) =>
      Proxy ei ->
      E en Composite Nothing kids cls eacs children ->
      E en Composite (Just ei) (AppendUniq ei kids) cls eacs children
    AppClsE :: (KnownSymbol en, KnownSymbol c) =>
      OrClass p c ->
      E en Composite ei kIds cls eacs children ->
      E en Composite ei kIds (c : cls) (ApplyClass p (C c) eacs) children
    AppendChildE :: (KnownSymbol ce, FindDup (MergeUniq ckIds pkIds) ~ Nothing) =>
      E ce cs ci ckIds ccls ceacs cchildren ->
      E pe Composite pi pkIds pcls peacs pchildren ->
      E pe Composite pi
      (MergeUniq ckIds pkIds)
      pcls
      (AppendChild
       pchildren
       ceacs
       (PrependMb
        (Fmap (TyCon I) pi)
        (T pe : SymsToSubSeg pcls))
       peacs)
      (PrependMb
        (Fmap (TyCon I) ci)
        (T ce : SymsToSubSeg ccls) : pchildren)

instance IsString (E CD Atomic Nothing (UnSet '[]) '[] '[] '[]) where
  fromString = CDataE . ms
