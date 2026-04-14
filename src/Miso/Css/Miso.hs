module Miso.Css.Miso where

import Data.Proxy ( Proxy(Proxy) )
import Data.Tagged ( Tagged(Tagged) )
import GHC.TypeLits ( KnownSymbol, symbolVal )
import Miso
    ( ms,
      vtext,
      MisoString,
      Attribute(Property, ClassList),
      View(VNode) )
import Miso.Html ( nodeHtml )
import Miso.Html.Property (id_)
import Miso.Css.List ( PrependMb, Append )
import Miso.Css.Segment ( SubSeg(T, R), ApplyClass, MbSymToMbI )
import Miso.Css.Style
    ( E(..),
      ElementStructure(Composite),
      OrClass,
      SymsToSubSeg,
      AppendChild,
      MapMaybeFilterOutFullyMatchedHead,
      Root(Root),
      BODY,
      HTML )
import Miso.Css.Prelude
    ( ($), Semigroup((<>)), Maybe(Nothing, Just) )



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

className :: forall p c. KnownSymbol c => OrClass p c -> MisoString
className _ =
  ms $ symbolVal $ Proxy @c

data Child

appChild :: Tagged Child (View model action) -> View model action -> View model action
appChild (Tagged c) = \case
  VNode ns tg atrs children -> VNode ns tg atrs $ children <> [c]
  o -> o

eToView :: E model action en es r ei knownIds cls eacs children -> View model action
eToView = \case
  RawMisoView rmv -> rmv
  CDataE txt -> vtext txt
  NilE enp -> nodeHtml (ms $ symbolVal enp) [] []
  IdE eni e -> injectElementId (ms $ symbolVal eni) (eToView e)
  AppClsE orCls e -> injectClass (className orCls) (eToView e)
  AppendChildE ce pe -> appChild (Tagged @Child (eToView ce)) (eToView pe)
  SealDomE e -> eToView e
  VirtualBodyE b -> eToView b

toView :: E model action en es r ei knownIds cls '[] children -> View model action
toView = eToView

body_ ::
  E model action ce   cs        Nothing      ci knownIds ccls ceacs cchildren ->
  E model action BODY Composite Nothing Nothing knownIds
    '[] -- classes
    (AppendChild
          '[]  -- pchildren
          ceacs
          '[ T BODY ]
          '[])
    '[ PrependMb (MbSymToMbI ci) (T ce : SymsToSubSeg ccls) ]
body_ = VirtualBodyE

html_ ::
  E model action ce   cs        Nothing      ci      ckids ccls ceacs cchildren ->
  E model action HTML Composite (Just 'Root) Nothing
    ckids
    '[]
    (MapMaybeFilterOutFullyMatchedHead
      '[]
      (ApplyClass '[] (T HTML) (ApplyClass '[] R ceacs)))
    '[ PrependMb (MbSymToMbI ci) (T ce : SymsToSubSeg ccls) ]
html_ = SealDomE

page ::
  E model action ce   cs         Nothing      ci      ckids ccls ceacs cchildren ->
  E model action HTML Composite  (Just 'Root) Nothing ckids '[]
    (MapMaybeFilterOutFullyMatchedHead
     '[]
     (ApplyClass
      '[] (T HTML)
      (ApplyClass
       '[] R
       (MapMaybeFilterOutFullyMatchedHead
        '[]
        (Append (ApplyClass '[] (T BODY) ceacs)
         '[])))))
    '[ '[T BODY]]
page x  = html_ (body_ x)
