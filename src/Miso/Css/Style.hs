-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}

module Miso.Css.Style where

import Data.List.Singletons ( NilSym0, type (:@#@$) )
import Data.Singletons.Base.TH
    ( Proxy, Tuple4Sym0, promote, TyCon, PFunctor(Fmap) )
import Data.String ( IsString(..) )
import GHC.TypeLits ( KnownSymbol, Symbol )
import Miso ( MisoString, ms, View )
import Miso.Css.List
    ( UnSet,
      Append,
      AppendSym0,
      append,
      UniqueSet,
      FindDup,
      AppendUniq,
      MergeUniq,
      PrependMb )
import Miso.Css.Segment
    ( SubSeg(..),
      MatchScope(JustNow, NowOrLater),
      Seg,
      AddSubSeg,
      BSym0,
      JustNowSym0,
      NowOrLaterSym0,
      ApplyClass )
import Miso.Css.Sibling
    ( matchSiblings, MatchSiblingsSym0, AddSiblingBr, SiblingBranch )
import Prelude

data AncestorClasses (p :: [Seg]) where
  CssOrphan :: Proxy ms -> AncestorClasses '[ '( ms, '[], '[], '[] )]
  AddRoot ::
    AncestorClasses ac -> AncestorClasses (AddSubSeg R ac)
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

type family AppendChild children ceacs pcls peacs where
  AppendChild children ceacs '[] peacs =
    MapMaybeFilterOutFullyMatchedHead children (Append ceacs peacs)
  AppendChild children ceacs (pclsH : pcls') peacs =
    AppendChild children (ApplyClass '[] pclsH ceacs) pcls' peacs

-- promoting not working
type family SymsToSubSeg l where
  SymsToSubSeg '[] = '[]
  SymsToSubSeg (h : l) = C h : SymsToSubSeg l

data ElementStructure = Atomic | Composite

type CD = "CDATA"
type RMV = "RawMisoView"
type HTML = "html"
type BODY = "body"
data Root = Root deriving (Show, Eq)

data E
     model
     action
     (en :: Symbol)
     (es :: ElementStructure)
     (re :: Maybe Root)
     (ei :: Maybe Symbol)
     (knownIds :: UniqueSet)
     (cls :: [Symbol])
     (l :: [[[Seg]]])
     (children :: [[SubSeg]])
  where
    RawMisoView ::
      View model action -> E model action RMV Atomic Nothing Nothing '[] '[] '[] '[]
    CDataE ::
      MisoString ->
      E model action CD Atomic Nothing Nothing (UnSet '[]) '[] '[] '[]
    NilE :: KnownSymbol en =>
      Proxy en ->
      E model action en Composite Nothing Nothing (UnSet '[]) '[] '[] '[]
    IdE :: (KnownSymbol ei, FindDup (AppendUniq ei kids) ~ Nothing) =>
      Proxy ei ->
      E model action en Composite r Nothing kids cls eacs children ->
      E model action en Composite r (Just ei) (AppendUniq ei kids) cls eacs children
    AppClsE :: (KnownSymbol en, KnownSymbol c) =>
      OrClass p c ->
      E model action en Composite r ei kIds cls eacs children ->
      E model action en Composite r ei kIds (c : cls) (ApplyClass p (C c) eacs) children
    AppendChildE :: (KnownSymbol ce, FindDup (MergeUniq ckIds pkIds) ~ Nothing) =>
      E model action ce cs Nothing ci ckIds ccls ceacs cchildren ->
      E model action pe Composite r pi pkIds pcls peacs pchildren ->
      E model action pe Composite r pi
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
    -- Miso can render view up to body to support :root the library provide
    -- VirtualBodyE and SealDomE to emulate top DOM elements (body and html) without
    -- generating them because the already exist
    VirtualBodyE ::
      E model action ce cs Nothing ci ckids ccls ceacs cchildren ->
      E model action BODY Composite Nothing Nothing
        ckids
        '[]
        (AppendChild
          '[]  -- pchildren
          ceacs
          '[ T BODY ]
          '[])
        '[ PrependMb (Fmap (TyCon I) ci) (T ce : SymsToSubSeg ccls) ]
    SealDomE ::
      E model action ce   cs        Nothing      ci      ckids ccls ceacs cchildren ->
      E model action HTML Composite (Just 'Root) Nothing
        ckids
        '[]
        (MapMaybeFilterOutFullyMatchedHead
          '[]
          (ApplyClass '[] (T HTML) (ApplyClass '[] R ceacs)))
        '[ PrependMb (Fmap (TyCon I) ci) (T ce : SymsToSubSeg ccls) ]

instance IsString (E model action CD Atomic Nothing Nothing (UnSet '[]) '[] '[] '[]) where
  fromString = CDataE . ms
