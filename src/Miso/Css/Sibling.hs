{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Miso.Css.Sibling where

import Data.Proxy ( Proxy )
import GHC.TypeLits (KnownSymbol)
import GHC.TypeError
    ( TypeError, ErrorMessage(Text, (:<>:), ShowType) )
import Miso.Css.List ( IsSubSet )
import Miso.Css.Segment
    ( SubSeg(B), Seg, MatchScope(NowOrLater, JustNow) )
import Prelude


type family MatchSiblingElemCaseMts mts p where
  MatchSiblingElemCaseMts JustNow p = Just '( JustNow, B : p)
  MatchSiblingElemCaseMts NowOrLater _p = Nothing

type family MatchSiblingElemCaseIsSubSet iss mts p where
  MatchSiblingElemCaseIsSubSet True  _   _ = '( True, Nothing)
  MatchSiblingElemCaseIsSubSet False mts p = '( False, MatchSiblingElemCaseMts mts p)

type family MatchSiblingElem el mts_p where
  MatchSiblingElem _el '( mts, B : p) = '( False, Just '( mts, B : p))
  MatchSiblingElem el  '( mts, p) =
    MatchSiblingElemCaseIsSubSet (IsSubSet p el) mts p

type family MatchSiblingBranchCase mser es p ps where
  MatchSiblingBranchCase '( True, _) es _p ps        = MatchSiblingBranch es ps
  MatchSiblingBranchCase '( False, Nothing) es p ps  = MatchSiblingBranch es (p:ps)
  MatchSiblingBranchCase '( False, Just p') _es p ps = p' : ps

type family MatchSiblingBranch es ps where
  MatchSiblingBranch _ '[] = '[]
  MatchSiblingBranch '[] p = '( JustNow, '[ B]) : p
  MatchSiblingBranch (e:es) (p:ps) =
    MatchSiblingBranchCase (MatchSiblingElem e p) es p ps

type family MatchSiblingsCase b r siblings bs where
  MatchSiblingsCase '[] _ _        _  = '[]
  MatchSiblingsCase b'  r siblings bs = MatchSiblings (b' : r) siblings bs

type family MatchSiblings r siblings bs where
  MatchSiblings r _ '[] = r
  MatchSiblings r siblings (b:bs) =
    MatchSiblingsCase (MatchSiblingBranch siblings b) r siblings bs

data Sibling (ms :: MatchScope) (ss :: [SubSeg]) where
  NilSib :: Proxy ms -> Sibling ms '[]
  AddSib :: KnownSymbol t => Proxy c -> Proxy t -> Sibling ms ss -> Sibling ms (c t : ss)

data SiblingBranch (sgs :: [(MatchScope, [SubSeg])]) where
  NilSibBranch :: SiblingBranch '[]
  AddSegToSibBranch :: Sibling ms ss -> SiblingBranch sgs -> SiblingBranch ( '( ms, ss) : sgs)

type family AddSiblingBr (sgs :: [(MatchScope, [SubSeg])]) (ac :: [Seg]) where
  AddSiblingBr sgs '[] =
    -- unreachable
    TypeError (Text "AddSiblingBr " :<>: ShowType sgs :<>: Text " to empty list")
  AddSiblingBr sgs ( '( mtScope, um, m, sib) : t) =
    ( '( mtScope, um, m,  sgs : sib) : t)
