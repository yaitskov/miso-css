{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Miso.Css.Segment where

import Miso.Css.List ( RemoveElem )
import Miso.Css.Prelude

data SubSeg
  = C Symbol -- ^ Element Class
  | T Symbol -- ^ Element Name (Tag)
  | I Symbol -- ^ Element Id
  | A Symbol -- ^ Attribute Name
  | R -- ^ CSS :root
  | B -- ^ Bottom is added to Seg to prevent matching branch later
      -- used to support CSS '>' syntax

type family MbSymToMbI mbs where
  MbSymToMbI Nothing = Nothing
  MbSymToMbI (Just s) = Just (I s)

data MatchScope
  = NowOrLater
  | JustNow
  | AutoClean -- ^ similar to JustNow but Segment is removed right away once UM list becomes empty.
              --   an empty JustNow segment lives until </, this way
              --   subseg constrains of tag to be applied
              -- why JustNow cannot be replaced with AutoClean?
              -- it cannot be replaced because following selector .a > p.c is indistiguishable from p.c.a
              -- Seg.siblings are empty for AutoClean
  deriving (Show, Eq)

-- | Composite segment
-- matched part is appended to unmatched when algorithm goes up to parent node
-- if unmatched is not empty
-- if unmatched is empty then list head is dropped
-- every SubSeg match step (ie class, id or tag name)
type Seg =
  ( MatchScope
  , [SubSeg] -- unmatched
  , [SubSeg] -- matched
  , [[(MatchScope, [SubSeg])]] -- siblings
  )

-- Initially Branch was [Seg] but it turned out that
-- selector ".a > .b" fails on hierarchy:
-- <p class="a">
--   <p class="b">
--     <p class="b">
-- The solution is to fork a branch if its head segment is JN when it matches
-- current element and match scope of previous segment in the branch is NL
newtype BRANCH = Branch [Seg]

type SegsOfBranch :: BRANCH -> [Seg]
type family SegsOfBranch b where
  SegsOfBranch (Branch sgs) = sgs
type family UnwrapBranchesInElem e where
  UnwrapBranchesInElem '[] = '[]
  UnwrapBranchesInElem (b : bs) = SegsOfBranch b : UnwrapBranchesInElem bs
type family UnwrapBranches es where
  UnwrapBranches '[] = '[]
  UnwrapBranches (e : es) = UnwrapBranchesInElem e : UnwrapBranches es

type SegsToBranch :: [Seg] -> BRANCH
type family SegsToBranch segs where
  SegsToBranch sgs = Branch sgs
type MkListOfranches :: [[Seg]] -> [BRANCH]
type family MkListOfranches segsList where
  MkListOfranches '[] = '[]
  MkListOfranches (segs : t) = SegsToBranch segs : MkListOfranches t


type AddSubSeg :: SubSeg -> [Seg] -> [Seg]
type family AddSubSeg c ac where
  AddSubSeg c '[] =
    -- unreachable
    TypeError (Text "AddSubSeg " :<>: ShowType c :<>: Text " to empty list")
  AddSubSeg c ( '( mtScope, um, m, sib) : t) =
    ( '( mtScope, c : um, m, sib) : t)

type family ApplySubSegToSeg removed ss sg where
  ApplySubSegToSeg Nothing          ss  h = h
  ApplySubSegToSeg (Just '(k, um')) _ss '( mts, um, m, sib) =
    '( mts, um', k : m, sib)

type ApplyClassToBranch :: SubSeg -> BRANCH -> BRANCH
type family ApplyClassToBranch subSeg sgs where
  ApplyClassToBranch _ (Branch '[]) = Branch '[]
  ApplyClassToBranch ss (Branch ( '( mts, um, m, sib) : t)) =
    Branch
      (ApplySubSegToSeg (RemoveElem '[] ss um) ss '( mts, um, m, sib) : t)


type ApplyClassToElem :: SubSeg -> [BRANCH]-> [BRANCH]
type family ApplyClassToElem c bs where
  ApplyClassToElem _ '[] = '[]
  ApplyClassToElem c  (b : bs) = ApplyClassToBranch c b : ApplyClassToElem c bs

type ApplyClass :: [[Seg]] -> SubSeg -> [[BRANCH]] -> [[BRANCH]]
type family ApplyClass acs c eacs where
  ApplyClass '[] _ '[] = '[]
  ApplyClass acs _ '[] = '[MkListOfranches acs]
  ApplyClass acs c (h : eacs) = ApplyClassToElem c h : ApplyClass acs c eacs

type ApplySubSegToBranch :: SubSeg -> [Seg] -> [Seg]
type family ApplySubSegToBranch subSeg sgs where
  ApplySubSegToBranch _ '[] = '[]
  ApplySubSegToBranch ss ( '( mts, um, m, sib) : t) =
    (ApplySubSegToSeg
      (RemoveElem '[] ss um)
      ss
      '( mts, um, m, sib) : t)

type family ApplySubSegToElem c bs where
  ApplySubSegToElem _ '[] = '[]
  ApplySubSegToElem c  (b : bs) =
    ApplySubSegToBranch c b : ApplySubSegToElem c bs

type ApplySubSegsToElem :: [SubSeg] -> [[Seg]] -> [[Seg]]
type family ApplySubSegsToElem ss bs where
  ApplySubSegsToElem '[] bs = bs
  ApplySubSegsToElem (h : t) bs = ApplySubSegsToElem t (ApplySubSegToElem h bs)
