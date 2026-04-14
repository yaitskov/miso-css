-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeAbstractions #-}

module Miso.Css.Style where

import Data.Proxy ( Proxy )
import Data.String ( IsString(..) )
import GHC.TypeLits ( KnownSymbol, Symbol )
import Miso ( MisoString, ms, View )
import Miso.Css.List
import Miso.Css.Segment
import Miso.Css.Sibling
    ( AddSiblingBr, SiblingBranch, MatchSiblings )
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

type family FilterOutFullyMatchedHeadCaseSibling
  siblingBranches siblings firstBranchTail r mts matched bs
  where
    FilterOutFullyMatchedHeadCaseSibling '[] _siblings _firstBranchTail _r  _mts _matched _bs = '[]
    FilterOutFullyMatchedHeadCaseSibling unmatchedSiblingBranches siblings firstBranchTail r mts matched bs =
      FilterOutFullyMatchedHead
        siblings
        (( '(mts, '[ B], matched, unmatchedSiblingBranches) : firstBranchTail) : r)
        bs

type family FilterOutFullyMatchedHead (siblings :: [[SubSeg]]) (r :: [[Seg]]) (bs :: [[Seg]]) where
  FilterOutFullyMatchedHead _siblings r '[] = r
  -- empty branch
  FilterOutFullyMatchedHead _siblings _r ('[] : _t) = '[]
  -- matched last branch segment
  FilterOutFullyMatchedHead _siblings _r ( '[ '( _mts, '[], _matched, '[]) ] : _bs) = '[]
  -- matched head seg in branch
  FilterOutFullyMatchedHead siblings r (( '(_mts, '[], _matched, '[]) : firstBranchTail) : bs) =
    FilterOutFullyMatchedHead siblings (firstBranchTail:r) bs
  -- filter siblings after all
  FilterOutFullyMatchedHead siblings r (( '(mts, '[], matched, siblingBranches) : firstBranchTail) : bs) =
    FilterOutFullyMatchedHeadCaseSibling
      (MatchSiblings '[] siblings siblingBranches)
      siblings firstBranchTail r mts matched bs

  -- skip locked
  FilterOutFullyMatchedHead siblings r (( '(_mts, B : unMatched, matched, sib) : firstBranchTail) : bs) =
    FilterOutFullyMatchedHead siblings (( '(_mts, B : unMatched, matched, sib) : firstBranchTail) : r) bs

  -- reset
  FilterOutFullyMatchedHead siblings r (( '(NowOrLater, unMatched, matched, sib) : firstBranchTail) : bs) =
    FilterOutFullyMatchedHead siblings (( '(NowOrLater, Append unMatched matched, '[], sib) : firstBranchTail) : r) bs

  -- lock
  FilterOutFullyMatchedHead siblings r (( '(JustNow, unMatched, matched, sib) : firstBranchTail) : bs) =
    FilterOutFullyMatchedHead siblings (( '(JustNow, B : unMatched, matched, sib) : firstBranchTail) : r) bs

type family MapMaybeFilterOutFullyMatchedHeadCase elems children t where
  MapMaybeFilterOutFullyMatchedHeadCase '[] children t =
    MapMaybeFilterOutFullyMatchedHead children t
  MapMaybeFilterOutFullyMatchedHeadCase h children t =
    h : MapMaybeFilterOutFullyMatchedHead children t

type family MapMaybeFilterOutFullyMatchedHead children eacs where
  MapMaybeFilterOutFullyMatchedHead _ '[] = '[]
  MapMaybeFilterOutFullyMatchedHead children (h : t) =
    MapMaybeFilterOutFullyMatchedHeadCase
      (FilterOutFullyMatchedHead children '[] h)
      children
      t

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
        (MbSymToMbI pi)
        (T pe : SymsToSubSeg pcls))
       peacs)
      (PrependMb
        (MbSymToMbI ci)
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
        '[ PrependMb (MbSymToMbI ci) (T ce : SymsToSubSeg ccls) ]
    SealDomE ::
      E model action ce   cs        Nothing      ci      ckids ccls ceacs cchildren ->
      E model action HTML Composite (Just 'Root) Nothing
        ckids
        '[]
        (MapMaybeFilterOutFullyMatchedHead
          '[]
          (ApplyClass '[] (T HTML) (ApplyClass '[] R ceacs)))
        '[ PrependMb (MbSymToMbI ci) (T ce : SymsToSubSeg ccls) ]

instance IsString (E model action CD Atomic Nothing Nothing (UnSet '[]) '[] '[] '[]) where
  fromString = CDataE . ms
