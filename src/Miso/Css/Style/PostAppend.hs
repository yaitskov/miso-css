{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Miso.Css.Style.PostAppend where

import Miso.Css.List ( Append )
import Miso.Css.Segment
    ( BRANCH(Branch),
      MatchScope(NowOrLater, JustNow, AutoClean),
      SubSeg(B) )
import Miso.Css.Sibling ( MatchSiblings )


type family FilterOutFullyMatchedHeadCaseSibling
  siblingBranches siblings firstBranchTail r mts matched bs
  where
    FilterOutFullyMatchedHeadCaseSibling '[] siblings firstBranchTail r  _mts _matched bs =
      FilterOutFullyMatchedHead
        siblings
        (Branch firstBranchTail NowOrLater : r)
        bs
    FilterOutFullyMatchedHeadCaseSibling unmatchedSiblingBranches siblings firstBranchTail r mts matched bs =
      FilterOutFullyMatchedHead
        siblings
        (Branch ('(mts, '[ B], matched, unmatchedSiblingBranches) : firstBranchTail) NowOrLater : r)
        bs

type FilterOutFullyMatchedHead :: [[SubSeg]] -> [BRANCH] -> [BRANCH] -> [BRANCH]
type family FilterOutFullyMatchedHead siblings r bs where
  FilterOutFullyMatchedHead _siblings r '[] = r
  -- empty branch
  FilterOutFullyMatchedHead _siblings _r (Branch '[] _ : _t) = '[]
  -- matched last branch segment
  FilterOutFullyMatchedHead _siblings _r
    (Branch '[ '( _mts, '[], _matched, '[]) ] _pms : _bs) = '[]

  -- reset segment and copy branch if previous tag relation in NOL
  -- and current relation is JN
  FilterOutFullyMatchedHead siblings r
    (Branch ( '(NowOrLater, '[], matched, '[])
            : '(JustNow, m_next, um_next, sib_next)
            : firstBranchTail
            ) pms
    : bs
    ) =
      FilterOutFullyMatchedHead
        siblings
        ( Branch ( '(JustNow, m_next, um_next, sib_next)
                 : firstBranchTail) pms
        : Branch ( '(NowOrLater, matched, '[], '[])
                 : '(JustNow, m_next, um_next, sib_next)
                 : firstBranchTail) pms
        : r
        ) bs

  -- matched head seg in branch
  FilterOutFullyMatchedHead siblings r
    (Branch ( '(mts, '[], _matched, '[]) : firstBranchTail) _pms : bs) =
      FilterOutFullyMatchedHead siblings (Branch firstBranchTail mts : r) bs
  -- filter siblings after all
  FilterOutFullyMatchedHead siblings r
    (Branch ( '(mts, '[], matched, siblingBranches) : firstBranchTail) pms : bs) =
      FilterOutFullyMatchedHeadCaseSibling
        (MatchSiblings '[] siblings siblingBranches)
        siblings firstBranchTail r mts matched bs

  -- skip locked
  FilterOutFullyMatchedHead siblings r
    (Branch ( '(_mts, B : unMatched, matched, sib) : firstBranchTail) pms : bs) =
      FilterOutFullyMatchedHead
        siblings
        (Branch ( '(_mts, B : unMatched, matched, sib) : firstBranchTail) pms : r) bs

  -- reset
  FilterOutFullyMatchedHead siblings r
    (Branch ( '(NowOrLater, unMatched, matched, sib) : firstBranchTail) pms : bs) =
      FilterOutFullyMatchedHead siblings
        (Branch ( '(NowOrLater, Append unMatched matched, '[], sib) : firstBranchTail) pms : r) bs

  -- lock
  FilterOutFullyMatchedHead siblings r
    (Branch ( '(JustNow, unMatched, matched, sib) : firstBranchTail) pms : bs) =
      FilterOutFullyMatchedHead siblings
        (Branch ( '(JustNow, B : unMatched, matched, sib) : firstBranchTail) pms : r) bs

  FilterOutFullyMatchedHead siblings r
    (Branch ( '(AutoClean, unMatched, matched, sib) : firstBranchTail) pms : bs) =
      FilterOutFullyMatchedHead siblings
        (Branch ( '(AutoClean, B : unMatched, matched, sib) : firstBranchTail) pms : r) bs

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
