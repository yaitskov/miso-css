{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Miso.Css.Style.E where

import Data.Proxy ( Proxy )
import Data.String ( IsString(..) )
import GHC.TypeLits ( KnownSymbol, Symbol )
import Miso ( MisoString, ms, View )
import Miso.Css.List
import Miso.Css.Segment
import Miso.Css.Style.OrClass ( OrClass )
import Miso.Css.Style.PostAppend qualified as Post
import Miso.Css.Style.PreAppend qualified as Pre
import Prelude

type family AppendChild children ceacs pcls peacs where
  AppendChild children ceacs '[] peacs =
    Post.MapMaybeFilterOutFullyMatchedHead children (Append ceacs peacs)
  AppendChild children ceacs (pclsH : pcls') peacs =
    AppendChild children (ApplyClass '[] pclsH ceacs) pcls' peacs

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
      E model action en Composite r (Just ei) (AppendUniq ei kids) cls
        (ApplyClass '[] (I ei) eacs)
        children
    AppClsE :: (KnownSymbol en, KnownSymbol c) =>
      OrClass p c ->
      E model action en Composite r ei kIds cls eacs children ->
      E model action en Composite r ei kIds (c : cls)
        (ApplyClass
          (ApplySubSegsToElem
             (PrependMb
               (MbSymToMbI ei)
               (T en : SymsToSubSeg cls))
             p)
          (C c)
          eacs)
        children
    AppendChildE :: (KnownSymbol ce, FindDup (MergeUniq ckIds pkIds) ~ Nothing) =>
      E model action ce cs Nothing ci ckIds ccls ceacs cchildren ->
      E model action pe Composite r pi pkIds pcls peacs pchildren ->
      E model action pe Composite r pi
      (MergeUniq ckIds pkIds)
      pcls
      (AppendChild
       pchildren
       (Pre.MapMaybeFilterOutFullyMatchedHead '[] ceacs)
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
        (Post.MapMaybeFilterOutFullyMatchedHead
          '[]
          (ApplyClass '[] (T HTML) (ApplyClass '[] R ceacs)))
        '[ PrependMb (MbSymToMbI ci) (T ce : SymsToSubSeg ccls) ]

instance IsString (E model action CD Atomic Nothing Nothing (UnSet '[]) '[] '[] '[]) where
  fromString = CDataE . ms
