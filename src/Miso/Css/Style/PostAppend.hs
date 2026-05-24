{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Miso.Css.Style.PostAppend where

import Miso.Css.List ( Append )
import Miso.Css.Segment
    ( MatchScope(AutoClean, NowOrLater, JustNow), SubSeg(B), Seg )
import Miso.Css.Sibling ( MatchSiblings )


type family FilterOutFullyMatchedHeadCaseSibling
  siblingBranches siblings firstBranchTail r mts matched bs
  where
    FilterOutFullyMatchedHeadCaseSibling '[] siblings firstBranchTail r  _mts _matched bs =
      FilterOutFullyMatchedHead siblings (firstBranchTail:r) bs -- []
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
  FilterOutFullyMatchedHead _siblings r (( '(_mts, B : unMatched, matched, sib) : firstBranchTail) : bs) =
    FilterOutFullyMatchedHead _siblings (( '(_mts, B : unMatched, matched, sib) : firstBranchTail) : r) bs

  -- reset
  FilterOutFullyMatchedHead _siblings r (( '(NowOrLater, unMatched, matched, sib) : firstBranchTail) : bs) =
    FilterOutFullyMatchedHead _siblings (( '(NowOrLater, Append unMatched matched, '[], sib) : firstBranchTail) : r) bs

  -- lock
  FilterOutFullyMatchedHead siblings r (( '(JustNow, unMatched, matched, sib) : firstBranchTail) : bs) =
    FilterOutFullyMatchedHead siblings (( '(JustNow, B : unMatched, matched, sib) : firstBranchTail) : r) bs

  FilterOutFullyMatchedHead _siblings r (( '(AutoClean, unMatched, matched, sib) : firstBranchTail) : bs) =
    FilterOutFullyMatchedHead _siblings (( '(AutoClean, B : unMatched, matched, sib) : firstBranchTail) : r) bs

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
