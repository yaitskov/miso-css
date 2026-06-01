{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Miso.Css.Style.PreAppend where

import Miso.Css.Segment
    ( BRANCH(Branch), MatchScope(AutoClean), SubSeg(B) )
import Miso.Css.Sibling ( MatchSiblings )


type family FilterOutFullyMatchedHeadCaseSibling
  siblingBranches siblings firstBranchTail r mts matched bs
  where
    FilterOutFullyMatchedHeadCaseSibling '[] siblings firstBranchTail r  _mts _matched bs =
      FilterOutFullyMatchedHead siblings (Branch firstBranchTail : r) bs
    FilterOutFullyMatchedHeadCaseSibling unmatchedSiblingBranches siblings firstBranchTail r mts matched bs =
      FilterOutFullyMatchedHead
        siblings
        (Branch ( '(mts, '[ B], matched, unmatchedSiblingBranches) : firstBranchTail) : r)

        bs

type FilterOutFullyMatchedHead :: [[SubSeg]] -> [BRANCH] -> [BRANCH] -> [BRANCH]
type family FilterOutFullyMatchedHead siblings r bs where
  FilterOutFullyMatchedHead _siblings r '[] = r
  -- empty branch
  FilterOutFullyMatchedHead _siblings _r (Branch '[] : _t) = '[]
  -- matched last branch segment
  FilterOutFullyMatchedHead _siblings _r
    (Branch '[ '(AutoClean, '[], _matched, '[]) ] : _bs) = '[]
  -- matched head seg in branch
  FilterOutFullyMatchedHead siblings r
    (Branch ( '(AutoClean, '[], _matched, '[]) : firstBranchTail) : bs) =
      FilterOutFullyMatchedHead siblings (Branch firstBranchTail : r) bs
  -- filter siblings after all
  FilterOutFullyMatchedHead siblings r
    (Branch ( '(AutoClean, '[], matched, siblingBranches) : firstBranchTail) : bs) =
      FilterOutFullyMatchedHeadCaseSibling
        (MatchSiblings '[] siblings siblingBranches)
        siblings firstBranchTail r AutoClean matched bs

  -- skip locked
  FilterOutFullyMatchedHead siblings r
    (Branch ( '(mts, B : unMatched, matched, sib) : firstBranchTail) : bs) =
      FilterOutFullyMatchedHead
        siblings
        (Branch ( '(mts, B : unMatched, matched, sib) : firstBranchTail) : r) bs

  -- lock
  FilterOutFullyMatchedHead siblings r
    (Branch ( '(AutoClean, unMatched, matched, sib) : firstBranchTail) : bs) =
      FilterOutFullyMatchedHead
        siblings
        (Branch ( '(AutoClean, B : unMatched, matched, sib) : firstBranchTail) : r) bs

  -- skip the rest (ie non AutoClean)
  FilterOutFullyMatchedHead siblings r
    (Branch ( '(mts, unMatched, matched, sib) : firstBranchTail) : bs) =
      FilterOutFullyMatchedHead
        siblings
        (Branch ( '(mts, unMatched, matched, sib) : firstBranchTail) : r) bs

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
