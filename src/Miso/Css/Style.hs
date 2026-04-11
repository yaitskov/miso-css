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
import GHC.TypeError
import GHC.TypeLits ( KnownSymbol, Symbol )
import Miso ( MisoString, ms )
import Miso.Css.List
import Prelude

$(promote
 [d|
  data SubSeg
    = C Symbol -- ^ Element Class
    | T Symbol -- ^ Element Name (Tag)
    | I Symbol -- ^ Element Id
    | B -- ^ Bottom is added to Seg to prevent matching branch later
        -- used to support CSS '>' syntax
    deriving (Show, Eq, Ord)
   |])

$(promote
 [d|
  data MatchScope = NowOrLater | JustNow deriving (Show, Eq)
   |])

-- | Composite segment
-- matched part is appended to unmatched when algorithm goes up to parent node
-- if unmatched is not empty
-- if unmatched is empty then list head is dropped
-- every SubSeg match step (ie class, id or tag name)
type Seg =
  ( MatchScope
  , [SubSeg] -- unmatched
  , [SubSeg] -- matched
  )

type family AddSubSeg (c :: SubSeg) (ac :: [Seg]) where
  AddSubSeg c '[] =
    -- unreachable
    TypeError (Text "AddSubSeg " :<>: ShowType c :<>: Text " to empty list")
  AddSubSeg c ( '( mtScope, um, m) : t) =
    ( '( mtScope, c : um, m) : t)

data AncestorClasses (p :: [Seg]) where
  CssOrphan :: Proxy ms -> AncestorClasses '[ '( ms, '[], '[] )]
  NextAncestor :: -- CSS star
    Proxy ms ->
    AncestorClasses ac ->
    AncestorClasses ('( ms, '[], '[]) : ac)
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
  applySubSegToSeg ss (mts, um, m) =
     case removeElem [] ss um of
       Nothing -> (mts, um, m)
       Just (k, um') -> (mts, um', k : m)
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
  filterOutFullyMatchedHead :: [[Seg]] -> [[Seg]] -> [[Seg]]
  filterOutFullyMatchedHead r [] = r
  -- empty branch
  filterOutFullyMatchedHead _r ([] : _t) = [] -- filterOutFullyMatchedHead t
  -- matched last branch segment
  filterOutFullyMatchedHead _r ( [ (_mts, [], _matched) ] : _bs) = []
  -- matched head seg in branch
  filterOutFullyMatchedHead r (((_mts, [], _matched) : firstBranchTail) : bs) =
    filterOutFullyMatchedHead (firstBranchTail:r) bs
  -- reset
  filterOutFullyMatchedHead r (((NowOrLater, unMatched, matched) : firstBranchTail) : bs) =
    filterOutFullyMatchedHead (((NowOrLater, unMatched `append` matched, []) : firstBranchTail) : r) bs

  filterOutFullyMatchedHead r (((JustNow, B : unMatched, matched) : firstBranchTail) : bs) =
    filterOutFullyMatchedHead (((JustNow, B : unMatched, matched) : firstBranchTail) : r) bs

  filterOutFullyMatchedHead r (((JustNow, unMatched, matched) : firstBranchTail) : bs) =
    filterOutFullyMatchedHead (((JustNow, B : unMatched, matched) : firstBranchTail) : r) bs
   |])

$(promote
 [d|
  mapMaybeFilterOutFullyMatchedHead :: [[[Seg]]] -> [[[Seg]]]
  mapMaybeFilterOutFullyMatchedHead [] = []
  mapMaybeFilterOutFullyMatchedHead (h:t) =
    case filterOutFullyMatchedHead [] h of
      [] -> mapMaybeFilterOutFullyMatchedHead t
      h' -> h' : mapMaybeFilterOutFullyMatchedHead t
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
