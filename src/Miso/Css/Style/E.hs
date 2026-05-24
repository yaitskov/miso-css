{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Miso.Css.Style.E where

import Data.Proxy ( Proxy(..) )
import Data.String ( IsString(..) )
import GHC.Generics (Generic)
import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Miso ( MisoString, ms, View )
import Miso.JSON ( ToJSON )
import Miso.Css.List
import Miso.Css.Segment
import Miso.Css.Style.OrClass ( OrClass )
import Miso.Css.Style.PostAppend qualified as Post
import Miso.Css.Style.PreAppend qualified as Pre
import Prelude

newtype ElAtr k v = ElAtr v deriving newtype (Eq, Ord, Show, ToJSON) deriving (Generic)

elAtrKey :: forall k v. KnownSymbol k => ElAtr k v -> MisoString
elAtrKey _ = ms $ symbolVal (Proxy @k)

elAtrVal :: forall {k1} {k2 :: k1} {v}. ElAtr k2 v -> v
elAtrVal (ElAtr v) = v

type family AppendChild children ceacs pcls peacs where
  AppendChild children ceacs '[] peacs =
    Post.MapMaybeFilterOutFullyMatchedHead children (Append ceacs peacs)
  AppendChild children ceacs (pclsH : pcls') peacs =
    AppendChild children (ApplyClass '[] pclsH ceacs) pcls' peacs

type family SymsToSubSeg l where
  SymsToSubSeg '[] = '[]
  SymsToSubSeg (h : l) = C h : SymsToSubSeg l

type family SymsToAtrs l where
  SymsToAtrs '[] = '[]
  SymsToAtrs (h : l) = A h : SymsToAtrs l

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
     (atrs :: [Symbol])
     (knownIds :: UniqueSet)
     (cls :: [Symbol])
     (l :: [[[Seg]]])
     (children :: [[SubSeg]])
  where
    RawMisoView ::
      View model action ->
      E model action RMV Atomic Nothing Nothing '[] '[] '[] '[] '[]
    CDataE ::
      MisoString ->
      E model action CD Atomic Nothing Nothing '[] (UnSet '[]) '[] '[] '[]
    NilE :: KnownSymbol en =>
      Proxy en ->
      E model action en Composite Nothing Nothing '[] (UnSet '[]) '[] '[] '[]
    AddAtrE :: (KnownSymbol k, ToJSON v) =>
      ElAtr k v ->
      E model action en Composite r ei atrs kids cls eacs children ->
      E model action en Composite r ei (k : atrs) kids cls
        (ApplyClass '[] (A k) eacs)
        children
    IdE :: (KnownSymbol ei, FindDup (AppendUniq ei kids) ~ Nothing) =>
      Proxy ei ->
      E model action en Composite r Nothing atrs kids cls eacs children ->
      E model action en Composite r (Just ei) ("id" : atrs) (AppendUniq ei kids) cls
        (ApplyClass '[] (A "id") (ApplyClass '[] (I ei) eacs))
        children
    AppClsE :: (KnownSymbol en, KnownSymbol c) =>
      OrClass p c ->
      E model action en Composite r ei atrs kIds cls eacs children ->
      E model action en Composite r ei ("class" : atrs) kIds (c : cls)
        (ApplyClass
          (ApplySubSegsToElem
             (PrependMb
               (MbSymToMbI ei)
               (T en : A "class" : Append (SymsToAtrs atrs) (SymsToSubSeg cls)))
             p)
          (C c)
          eacs)
        children
    AppendChildE :: (KnownSymbol ce, FindDup (MergeUniq ckIds pkIds) ~ Nothing) =>
      E model action ce cs Nothing ci cAtrs ckIds ccls ceacs cchildren ->
      E model action pe Composite r pi pAtrs pkIds pcls peacs pchildren ->
      E model action pe Composite r pi pAtrs
      (MergeUniq ckIds pkIds)
      pcls
      (AppendChild
       pchildren
       (Pre.MapMaybeFilterOutFullyMatchedHead '[] ceacs)
       (PrependMb
        (MbSymToMbI pi)
        (T pe : Append (SymsToAtrs pAtrs) (SymsToSubSeg pcls)))
       peacs)
      (PrependMb
        (MbSymToMbI ci)
        (T ce : SymsToSubSeg ccls) : pchildren)
    -- Miso can render view up to body to support :root the library provide
    -- VirtualBodyE and SealDomE to emulate top DOM elements (body and html) without
    -- generating them because the already exist
    VirtualBodyE ::
      E model action ce cs Nothing ci catrs ckids ccls ceacs cchildren ->
      E model action BODY Composite Nothing Nothing catrs
        ckids
        '[]
        (AppendChild
          '[]  -- pchildren
          ceacs
          '[ T BODY ]
          '[])
        '[ PrependMb (MbSymToMbI ci) (T ce : SymsToSubSeg ccls) ]
    SealDomE ::
      E model action ce   cs        Nothing      ci      catrs ckids ccls ceacs cchildren ->
      E model action HTML Composite (Just 'Root) Nothing catrs
        ckids
        '[]
        (Post.MapMaybeFilterOutFullyMatchedHead
          '[]
          (ApplyClass '[] (T HTML) (ApplyClass '[] R ceacs)))
        '[ PrependMb (MbSymToMbI ci) (T ce : SymsToSubSeg ccls) ]

instance IsString (E model action CD Atomic Nothing Nothing '[] (UnSet '[]) '[] '[] '[]) where
  fromString = CDataE . ms
