module Miso.Css.Miso where

import GHC.TypeLits (natVal, charVal)
import Miso
    ( ms,
      vtext,
      prop,
      MisoString,
      Attribute(Property, ClassList),
      View(VNode) )
import Miso.JSON ( ToJSON(..), Value(String) )
import Miso.Html ( nodeHtml )
import Miso.Html.Property (id_)
import Miso.Css.Event ( EventFactory(mkActionAttribute) )
import Miso.Css.List ( PrependMb, Append )
import Miso.Css.Segment
    ( ApplyClass,
      MbSymToMbI,
      SubSeg(T, R),
      UnwrapBranches,
      RemoveMatchScope )
import Miso.Css.Style
import Miso.Css.Style.PostAppend
    ( MapMaybeFilterOutFullyMatchedHead )
import Miso.Css.Prelude

injectClass :: MisoString
  -> View ctx props model action
  -> View ctx props model action
injectClass cn = \case
  VNode ns tg atrs children dirEvents ->
    VNode ns tg (injClass atrs) children dirEvents
  o -> o
  where
    injClass = \case
      [] -> [ClassList [cn]]
      ClassList cls : atrs' -> ClassList (cls <> [cn]) : atrs'
      o : atrs' -> o : injClass atrs'

injectElementId :: MisoString
  -> View ctx props model action
  -> View ctx props model action
injectElementId ik = \case
  VNode ns tg atrs children dirEvents ->
    VNode ns tg (injId atrs) children dirEvents
  o -> o
  where
    injId = \case
      [] -> [id_ ik]
      -- Property "key" _v : atrs' -> key_ ik : atrs'
      Property "id" _v : atrs' -> id_ ik : atrs'
      o : atrs' -> o : injId atrs'

injectElementAtr :: ToJSON v
  => MisoString
  -> v
  -> View ctx props model action
  -> View ctx props model action
injectElementAtr ik v = \case
  VNode ns tg atrs children dirEvents ->
    VNode ns tg (injId atrs) children dirEvents
  o -> o
  where
    p = prop ik v
    injId = \case
      [] -> [p]
      -- Property "key" _v : atrs' -> key_ ik : atrs'
      Property pn pnv : atrs'
        | pn == ik -> Property pn (toJSON v) : atrs'
        | otherwise -> Property pn pnv : injId atrs'
      o : atrs' -> o : injId atrs'


injAppElemAtr :: ToJSON v
  => MisoString -> v
  -> View ctx props model action
  -> View ctx props model action
injAppElemAtr ik v = \case
  VNode ns tg atrs children dirEvents ->
    VNode ns tg (injId atrs) children dirEvents
  o -> o
  where
    p = prop ik v
    injId = \case
      [] -> [p]
      Property pn (String pnv) : atrs'
        | pn == ik ->
          case toJSON v of
            String vs -> Property pn (String $ pnv <> " " <> vs) : atrs'
            o -> error $ "expected String but: " <> show o <> "; " <> show ik
        | otherwise -> Property pn (String pnv) : injId atrs'
      Property pn pnv : atrs'
        | pn == ik -> Property pn (toJSON v) : atrs'
        | otherwise -> Property pn pnv : injId atrs'
      o : atrs' -> o : injId atrs'

injectEventHandler :: EventFactory ef model action
  => ef
  -> View ctx props model action
  -> View ctx props model action
injectEventHandler ef = \case
  VNode ns tg atrs children dirEvents ->
    VNode ns tg (mkActionAttribute ef : atrs) children dirEvents
  o -> o

className :: forall p c. KnownSymbol c => OrClass p c -> MisoString
className _ =
  ms $ symbolVal $ Proxy @c

data Child

appChild :: Tagged Child (View ctx props model action)
  -> View ctx props model action
  -> View ctx props model action
appChild (Tagged c) = \case
  VNode ns tg atrs children dirEvents -> VNode ns tg atrs (children <> [c]) dirEvents
  o -> o

eToView :: E ctx props model action en es r ei atrs knownIds cls eacs children for tabx ak
  -> View ctx props model action
eToView = \case
  RawMisoView rmv -> rmv
  CDataE txt -> vtext txt
  NilE enp -> nodeHtml (ms $ symbolVal enp) [] []
  IdE eni e -> injectElementId (ms $ symbolVal eni) (eToView e)
  AppClsE orCls e -> injectClass (className orCls) (eToView e)
  AppendChildE ce pe -> appChild (Tagged @Child (eToView ce)) (eToView pe)
  TabIndexMinus1E e -> injectElementAtr "tabindex" ("-1" :: MisoString) (eToView e)
  AccessKeyE nc e -> injectElementAtr "accesskey" (ms [charVal nc]) (eToView e)
  TabIndexE ti e -> injectElementAtr "tabindex" (show $ natVal ti) (eToView e)
  AddAtrE a e -> injectElementAtr (elAtrKey a) (elAtrVal a) (eToView e)
  AddForAtrE f e -> injectElementAtr "for" (ms $ symbolVal f) (eToView e)
  AddAriaIdAtrE aria fi e -> injAppElemAtr (ms $ symbolVal aria) (ms $ symbolVal fi) (eToView e)
  BindEventE ef e -> injectEventHandler ef (eToView e)
  SealDomE e -> eToView e
  VirtualBodyE b -> eToView b

toView :: forall ctx prs m a en es r ei atrs kids cls ecs children for tabx ak.
  ( RemoveMatchScope (UnwrapBranches (MapMaybeFilterOutFullyMatchedHead '[] ecs)) ~ '[]
  , DuplicatedIds kids ~ '[]
  , for ~ '[]
  , TabIndexesConsistent tabx ~ '[]
  , AccessKeyConsistent ak ~ '[]
  )
  => E ctx prs m a en es r ei atrs kids cls ecs children for tabx ak
  -> View ctx prs m a
toView = eToView

-- | More detailed types that 'toView'
toView' :: forall ctx prs m a en es r ei atrs kids cls ecs children for tabx ak.
  ( UnwrapBranches (MapMaybeFilterOutFullyMatchedHead '[] ecs) ~ '[]
  , DuplicatedIds kids ~ '[]
  , for ~ '[]
  , TabIndexesConsistent tabx ~ '[]
  , AccessKeyConsistent ak ~ '[]
  )
  => E ctx prs m a en es r ei atrs kids cls ecs children for tabx ak
  -> View ctx prs m a
toView' = eToView

body_ ::
  E ctx props model action
      ce   cs        Nothing      ci atrs knownIds ccls ceacs cchildren cfor tabx ak ->
  E ctx props model action
      BODY Composite Nothing Nothing atrs knownIds '[]
    (AppendChild
          '[]  -- pchildren
          ceacs
          '[ T BODY ]
          '[])
    '[ PrependMb (MbSymToMbI ci) (T ce : SymsToSubSeg ccls) ]
    cfor tabx ak
body_ = VirtualBodyE

html_ ::
  E ctx props model action
      ce   cs        Nothing      ci      catrs ckids ccls ceacs cchildren cfor tabx ak ->
  E ctx props model action
      HTML Composite (Just 'Root) Nothing catrs ckids '[]
    (MapMaybeFilterOutFullyMatchedHead
      '[]
      (ApplyClass '[] (T HTML) (ApplyClass '[] R ceacs)))
    '[ PrependMb (MbSymToMbI ci) (T ce : SymsToSubSeg ccls) ]
    cfor tabx ak
html_ = SealDomE

page ::
  E ctx props model action
      ce   cs         Nothing      ci      catrs ckids ccls ceacs cchildren cfor tabx ak ->
  E ctx props model action
      HTML Composite  (Just 'Root) Nothing catrs ckids '[]
    (MapMaybeFilterOutFullyMatchedHead
     '[]
     (ApplyClass
      '[] (T HTML)
      (ApplyClass
       '[] R
       (Append
        (MapMaybeFilterOutFullyMatchedHead
          '[]
          (ApplyClass '[] (T BODY) ceacs))
         '[]))))
    '[ '[T BODY]]
    cfor tabx ak
page x  = html_ (body_ x)
