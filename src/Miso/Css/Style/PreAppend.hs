{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Miso.Css.Style.PreAppend where

import Miso.Css.Segment ( MatchScope(AutoClean), SubSeg(B), Seg )
import Miso.Css.Sibling ( MatchSiblings )


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
  FilterOutFullyMatchedHead _siblings _r ( '[ '(AutoClean, '[], _matched, '[]) ] : _bs) = '[]
  -- matched head seg in branch
  FilterOutFullyMatchedHead siblings r (( '(AutoClean, '[], _matched, '[]) : firstBranchTail) : bs) =
    FilterOutFullyMatchedHead siblings (firstBranchTail:r) bs
  -- filter siblings after all
  FilterOutFullyMatchedHead siblings r (( '(AutoClean, '[], matched, siblingBranches) : firstBranchTail) : bs) =
    FilterOutFullyMatchedHeadCaseSibling
      (MatchSiblings '[] siblings siblingBranches)
      siblings firstBranchTail r AutoClean matched bs

  -- skip locked
  FilterOutFullyMatchedHead _siblings r (( '(mts, B : unMatched, matched, sib) : firstBranchTail) : bs) =
    FilterOutFullyMatchedHead _siblings (( '(mts, B : unMatched, matched, sib) : firstBranchTail) : r) bs

  -- lock
  FilterOutFullyMatchedHead _siblings r (( '(AutoClean, unMatched, matched, sib) : firstBranchTail) : bs) =
    FilterOutFullyMatchedHead _siblings (( '(AutoClean, B : unMatched, matched, sib) : firstBranchTail) : r) bs

  -- skip the rest (ie non AutoClean)
  FilterOutFullyMatchedHead _siblings r (( '(mts, unMatched, matched, sib) : firstBranchTail) : bs) =
    FilterOutFullyMatchedHead _siblings (( '(mts, unMatched, matched, sib) : firstBranchTail) : r) bs


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
