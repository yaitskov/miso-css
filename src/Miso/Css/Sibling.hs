{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}

module Miso.Css.Sibling where

import Data.Proxy
import Data.Maybe.Singletons ( JustSym0, NothingSym0 )
import Data.List.Singletons ( NilSym0, type (:@#@$) )
import Data.Singletons.Base.TH
    ( promote, TrueSym0, FalseSym0, Tuple2Sym0 )
import GHC.TypeError
import Miso.Css.List ( isSubSet, IsSubSetSym0 )
import Miso.Css.Segment
import Prelude
import GHC.TypeLits (KnownSymbol)


$(promote
 [d|
  matchSiblingElem :: [SubSeg] -> (MatchScope, [SubSeg]) -> (Bool, Maybe (MatchScope, [SubSeg]))
  matchSiblingElem _el (mts, B : p) = (False, Just (mts, B : p))
  matchSiblingElem el (mts, p)
   | isSubSet p el = (True, Nothing)
   | otherwise =
     ( False
     , case mts of
         JustNow -> Just (JustNow, B : p)
         NowOrLater -> Nothing
     )
   |])

$(promote
 [d|
  matchSiblingBranch :: [[SubSeg]] -> [(MatchScope, [SubSeg])] -> [(MatchScope, [SubSeg])]
  matchSiblingBranch _ [] = []
  matchSiblingBranch [] p = (JustNow, [B]) : p
  matchSiblingBranch (e:es) (p:ps) =
     case matchSiblingElem e p of
       (True, _) -> matchSiblingBranch es ps
       (False, Nothing) -> matchSiblingBranch es (p:ps)
       (False, Just p') -> p' : ps
   |])

$(promote
 [d|
  matchSiblings :: [[(MatchScope, [SubSeg])]] -> [[SubSeg]] -> [[(MatchScope, [SubSeg])]] -> [[(MatchScope, [SubSeg])]]
  matchSiblings r _ [] = r
  matchSiblings r siblings (b:bs) =
    case matchSiblingBranch siblings b of
      [] -> [] -- branch fully matched -> element matches
      b' -> matchSiblings (b':r) siblings bs
  |])

data Sibling (ms :: MatchScope) (ss :: [SubSeg]) where
  NilSib :: Proxy ms -> Sibling ms '[]
  AddTagToSib :: KnownSymbol t => Proxy t -> Sibling ms ss -> Sibling ms (T t : ss)
  AddIdToSib :: KnownSymbol t => Proxy t -> Sibling ms ss -> Sibling ms (I t : ss)
  AddClassToSib :: KnownSymbol t => Proxy t -> Sibling ms ss -> Sibling ms (C t : ss)

data SiblingBranch (sgs :: [(MatchScope, [SubSeg])]) where
  NilSibBranch :: SiblingBranch '[]
  AddSegToSibBranch :: Sibling ms ss -> SiblingBranch sgs -> SiblingBranch ( '( ms, ss) : sgs)

type family AddSiblingBr (sgs :: [(MatchScope, [SubSeg])]) (ac :: [Seg]) where
  AddSiblingBr sgs '[] =
    -- unreachable
    TypeError (Text "AddSiblingBr " :<>: ShowType sgs :<>: Text " to empty list")
  AddSiblingBr sgs ( '( mtScope, um, m, sib) : t) =
    ( '( mtScope, um, m,  sgs : sib) : t)
