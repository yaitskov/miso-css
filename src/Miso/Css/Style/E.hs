{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Miso.Css.Style.E where

import Data.String ( IsString(..) )
import Data.Type.Bool ( If )
import GHC.Generics (Generic)
import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Miso ( MisoString, ms, View )
import Miso.Css.Event (EventFactory)
import Miso.Css.List ( MergeUniq, Elem, PrependMb, Append )
import Miso.Css.Prelude
import Miso.Css.Segment
    ( SubSeg(C, R, I, T, A),
      ApplySubSegsToElem,
      ApplyClass,
      Seg,
      MbSymToMbI )
import Miso.Css.Style.OrClass ( OrClass )
import Miso.Css.Style.PostAppend qualified as Post
import Miso.Css.Style.PreAppend qualified as Pre
import Miso.JSON ( ToJSON )



newtype ElAtr k v = ElAtr v deriving newtype (Eq, Ord, Show, ToJSON) deriving (Generic)

elAtrKey :: forall k v. KnownSymbol k => ElAtr k v -> MisoString
elAtrKey _ = ms $ symbolVal (Proxy @k)

elAtrVal :: forall {k1} {k2 :: k1} {v}. ElAtr k2 v -> v
elAtrVal (ElAtr v) = v

type family AppendChild children ceacs pcls peacs where
  AppendChild children ceacs '[] peacs =
    (Append (Post.MapMaybeFilterOutFullyMatchedHead children ceacs) peacs)
  AppendChild children ceacs (pclsH : pcls') peacs =
    AppendChild children (ApplyClass '[] pclsH ceacs) pcls' peacs

type family SymsToSubSeg l where
  SymsToSubSeg '[] = '[]
  SymsToSubSeg (h : l) = C h : SymsToSubSeg l

type family SymsToAtrs l where
  SymsToAtrs '[] = '[]
  SymsToAtrs (h : l) = A h : SymsToAtrs l

data ElementStructure = Atomic | Composite

newtype DuplicatedID = DuplicatedId Symbol

data KnownIDS = KnownIds { duplicatedIds :: [DuplicatedID], knownIds :: [Symbol] }

type family DuplicatedIds kids where
  DuplicatedIds (KnownIds dids _) = dids

type family AddTagId x kids where
  AddTagId x (KnownIds dids kids) =
    If (Elem x kids)
      (KnownIds (DuplicatedId x : dids) kids)
      (KnownIds dids (x : kids))

type family MergeKidsCase r didsA didsB kidsA kidsB where
  MergeKidsCase (Left e) didsA didsB kidsA kidsB =
    KnownIds
      (DuplicatedId e : Append didsA didsB)
      (Append kidsA kidsB)
  MergeKidsCase (Right kidsAB) didsA didsB _kidsA _kidsB =
    KnownIds
      (Append didsA didsB)
      kidsAB

type family MergeKids a b where
  MergeKids (KnownIds didsA kidsA) (KnownIds didsB kidsB) =
    MergeKidsCase (MergeUniq kidsA kidsB '[]) didsA didsB kidsA kidsB

type EmptyKids = KnownIds '[] '[]

type CD = "CDATA"
type RMV = "RawMisoView"
type HTML = "html"
type BODY = "body"
data Root = Root deriving (Show, Eq)

type family ConstraintsAfterAppend pchildren ceacs pi pe pAtrs pcls peacs where
  ConstraintsAfterAppend pchildren ceacs pi pe pAtrs pcls peacs =
    AppendChild
      pchildren
      (Pre.MapMaybeFilterOutFullyMatchedHead '[] ceacs)
      (PrependMb
        (MbSymToMbI pi)
        (T pe : Append (SymsToAtrs pAtrs) (SymsToSubSeg pcls)))
      peacs

type family ChildrenConstrAfterAppend ci ce ccls pchildren where
  ChildrenConstrAfterAppend ci ce ccls pchildren =
    PrependMb (MbSymToMbI ci) (T ce : SymsToSubSeg ccls) : pchildren

type family ConstraintsAfterClassApp ei en atrs cls p c eacs where
  ConstraintsAfterClassApp ei en atrs cls p c eacs =
    ApplyClass
      (ApplySubSegsToElem
        (PrependMb
          (MbSymToMbI ei)
          (T en : A "class" : Append (SymsToAtrs atrs) (SymsToSubSeg cls)))
        p)
      (C c)
      eacs

type family ConstraintsAfteId ei eacs where
  ConstraintsAfteId ei eacs = ApplyClass '[] (A "id") (ApplyClass '[] (I ei) eacs)

data E
     model
     action
     (en :: Symbol)
     (es :: ElementStructure)
     (re :: Maybe Root)
     (ei :: Maybe Symbol)
     (atrs :: [Symbol])
     (knownIds :: KnownIDS)
     (cls :: [Symbol])
     (l :: [[[Seg]]])
     (children :: [[SubSeg]])
  where
    RawMisoView ::
      View model action ->
      E model action RMV Atomic Nothing Nothing '[] EmptyKids '[] '[] '[]
    CDataE ::
      MisoString ->
      E model action CD Atomic Nothing Nothing '[] EmptyKids '[] '[] '[]
    NilE :: KnownSymbol en =>
      Proxy en ->
      E model action en Composite Nothing Nothing '[] EmptyKids '[] '[] '[]
    AddAtrE :: (KnownSymbol k, ToJSON v) =>
      ElAtr k v ->
      E model action en Composite r ei atrs kids cls eacs children ->
      E model action en Composite r ei (k : atrs) kids cls
        (ApplyClass '[] (A k) eacs)
        children
    IdE :: KnownSymbol ei =>
      Proxy ei ->
      E model action en Composite r Nothing atrs kids cls eacs children ->
      E model action en Composite r (Just ei) ("id" : atrs)
        (AddTagId ei kids)
        cls
        (ConstraintsAfteId ei eacs)
        children
    BindEventE :: EventFactory ef action =>
      ef ->
      E model action en es r ei atrs kids cls eacs children ->
      E model action en es r ei atrs kids cls eacs children
    AppClsE :: (KnownSymbol en, KnownSymbol c) =>
      OrClass p c ->
      E model action en Composite r ei atrs kIds cls eacs children ->
      E model action en Composite r ei ("class" : atrs) kIds (c : cls)
        (ConstraintsAfterClassApp ei en atrs cls p c eacs)
        children
    AppendChildE :: KnownSymbol ce =>
      E model action ce cs Nothing ci cAtrs ckIds ccls ceacs cchildren ->
      E model action pe Composite r pi pAtrs pkIds pcls peacs pchildren ->
      E model action pe Composite r pi pAtrs
      (MergeKids ckIds pkIds)
      pcls
      (ConstraintsAfterAppend pchildren ceacs pi pe pAtrs pcls peacs)
      (ChildrenConstrAfterAppend ci ce ccls pchildren)
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

instance IsString (E model action CD Atomic Nothing Nothing '[] EmptyKids '[] '[] '[]) where
  fromString = CDataE . ms
