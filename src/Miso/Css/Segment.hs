{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Miso.Css.Segment where

import Data.List.Singletons
import Data.Singletons.Base.TH
import GHC.TypeError
    ( TypeError, ErrorMessage(Text, (:<>:), ShowType) )
import GHC.TypeLits ( Symbol )
import Miso.Css.List ( removeElem, RemoveElemSym0 )
import Prelude

$(promote
 [d|
  data SubSeg
    = C Symbol -- ^ Element Class
    | T Symbol -- ^ Element Name (Tag)
    | I Symbol -- ^ Element Id
    | R -- ^ CSS :root
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
  , [[(MatchScope, [SubSeg])]] -- siblings
  )

type family AddSubSeg (c :: SubSeg) (ac :: [Seg]) where
  AddSubSeg c '[] =
    -- unreachable
    TypeError (Text "AddSubSeg " :<>: ShowType c :<>: Text " to empty list")
  AddSubSeg c ( '( mtScope, um, m, sib) : t) =
    ( '( mtScope, c : um, m, sib) : t)

$(promote
 [d|
  applySubSegToSeg :: SubSeg -> Seg -> Seg
  applySubSegToSeg ss (mts, um, m, sib) =
     case removeElem [] ss um of
       Nothing -> (mts, um, m, sib)
       Just (k, um') -> (mts, um', k : m, sib)
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
