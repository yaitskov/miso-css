{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Miso.Css.Segment where

import GHC.TypeError
    ( TypeError, ErrorMessage(Text, (:<>:), ShowType) )
import GHC.TypeLits ( Symbol )
import Miso.Css.List ( RemoveElem )
import Prelude

data SubSeg
  = C Symbol -- ^ Element Class
  | T Symbol -- ^ Element Name (Tag)
  | I Symbol -- ^ Element Id
  | R -- ^ CSS :root
  | B -- ^ Bottom is added to Seg to prevent matching branch later
      -- used to support CSS '>' syntax

type family MbSymToMbI mbs where
  MbSymToMbI Nothing = Nothing
  MbSymToMbI (Just s) = Just (I s)

data MatchScope = NowOrLater | JustNow deriving (Show, Eq)

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

type family AddSubSeg (c :: SubSeg) (ac :: [Seg]) where
  AddSubSeg c '[] =
    -- unreachable
    TypeError (Text "AddSubSeg " :<>: ShowType c :<>: Text " to empty list")
  AddSubSeg c ( '( mtScope, um, m, sib) : t) =
    ( '( mtScope, c : um, m, sib) : t)

type family ApplySubSegToSeg removed ss sg where
  ApplySubSegToSeg Nothing          ss  h = h
  ApplySubSegToSeg (Just '(k, um')) _ss '( mts, um, m, sib) =
    '( mts, um', k : m, sib)

type family ApplyClassToBranch subSeg sgs where
  ApplyClassToBranch _ '[] = '[]
  ApplyClassToBranch ss ( '( mts, um, m, sib) : t) =
    ApplySubSegToSeg (RemoveElem '[] ss um) ss '( mts, um, m, sib) : t

type family ApplyClassToElem c bs where
  ApplyClassToElem _ '[] = '[]
  ApplyClassToElem c  (b : bs) = ApplyClassToBranch c b : ApplyClassToElem c bs

type family ApplyClass acs c eacs where
  ApplyClass '[] _ '[] = '[]
  ApplyClass acs _ '[] = '[ acs]
  ApplyClass acs c (h : eacs) = ApplyClassToElem c h : ApplyClass acs c eacs
