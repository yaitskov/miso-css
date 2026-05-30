module Miso.Css.Miso where

import GHC.TypeLits ( KnownSymbol, symbolVal )
import Miso
    ( ms,
      vtext,
      prop,
      MisoString,
      Attribute(Property, ClassList),
      View(VNode) )
import Miso.JSON ( ToJSON(..) )
import Miso.Html ( nodeHtml )
import Miso.Html.Property (id_)
import Miso.Css.Event ( EventFactory(mkActionAttribute) )
import Miso.Css.List ( PrependMb, Append )
import Miso.Css.Segment ( SubSeg(T, R), ApplyClass, MbSymToMbI )
import Miso.Css.Style
import Miso.Css.Style.PostAppend
    ( MapMaybeFilterOutFullyMatchedHead )
import Miso.Css.Prelude

injectClass :: MisoString -> View model action -> View model action
injectClass cn = \case
  VNode ns tg atrs children -> VNode ns tg (injClass atrs) children
  o -> o
  where
    injClass = \case
      [] -> [ClassList [cn]]
      ClassList cls : atrs' -> ClassList (cls <> [cn]) : atrs'
      o : atrs' -> o : injClass atrs'

injectElementId :: MisoString -> View model action -> View model action
injectElementId ik = \case
  VNode ns tg atrs children -> VNode ns tg (injId atrs) children
  o -> o
  where
    injId = \case
      [] -> [id_ ik]
      -- Property "key" _v : atrs' -> key_ ik : atrs'
      Property "id" _v : atrs' -> id_ ik : atrs'
      o : atrs' -> o : injId atrs'

injectElementAtr :: ToJSON v => MisoString -> v -> View model action -> View model action
injectElementAtr ik v = \case
  VNode ns tg atrs children -> VNode ns tg (injId atrs) children
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

injectEventHandler :: EventFactory ef action =>
  ef -> View model action -> View model action
injectEventHandler ef = \case
  VNode ns tg atrs children ->
    VNode ns tg (mkActionAttribute ef : atrs) children
  o -> o

className :: forall p c. KnownSymbol c => OrClass p c -> MisoString
className _ =
  ms $ symbolVal $ Proxy @c

data Child

appChild :: Tagged Child (View model action) -> View model action -> View model action
appChild (Tagged c) = \case
  VNode ns tg atrs children -> VNode ns tg atrs $ children <> [c]
  o -> o

eToView :: E model action en es r ei atrs knownIds cls eacs children -> View model action
eToView = \case
  RawMisoView rmv -> rmv
  CDataE txt -> vtext txt
  NilE enp -> nodeHtml (ms $ symbolVal enp) [] []
  IdE eni e -> injectElementId (ms $ symbolVal eni) (eToView e)
  AppClsE orCls e -> injectClass (className orCls) (eToView e)
  AppendChildE ce pe -> appChild (Tagged @Child (eToView ce)) (eToView pe)
  AddAtrE a e -> injectElementAtr (elAtrKey a) (elAtrVal a) (eToView e)
  BindEventE ef e -> injectEventHandler ef (eToView e)
  SealDomE e -> eToView e
  VirtualBodyE b -> eToView b

toView :: forall m a en es r ei atrs kids cls ecs children.
  ( MapMaybeFilterOutFullyMatchedHead '[] ecs ~ '[]
  , DuplicatedIds kids ~ '[]
  ) =>
  E m a en es r ei atrs kids cls ecs children -> View m a
toView = eToView

body_ ::
  E model action ce   cs        Nothing      ci atrs knownIds ccls ceacs cchildren ->
  E model action BODY Composite Nothing Nothing atrs knownIds
    '[] -- classes
    (AppendChild
          '[]  -- pchildren
          ceacs
          '[ T BODY ]
          '[])
    '[ PrependMb (MbSymToMbI ci) (T ce : SymsToSubSeg ccls) ]
body_ = VirtualBodyE

html_ ::
  E model action ce   cs        Nothing      ci      catrs ckids ccls ceacs cchildren ->
  E model action HTML Composite (Just 'Root) Nothing
    catrs ckids
    '[]
    (MapMaybeFilterOutFullyMatchedHead
      '[]
      (ApplyClass '[] (T HTML) (ApplyClass '[] R ceacs)))
    '[ PrependMb (MbSymToMbI ci) (T ce : SymsToSubSeg ccls) ]
html_ = SealDomE

page ::
  E model action ce   cs         Nothing      ci      catrs ckids ccls ceacs cchildren ->
  E model action HTML Composite  (Just 'Root) Nothing catrs ckids '[]
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
page x  = html_ (body_ x)
