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
import Miso.Css.Style ( OrClass, E(..) )
import Miso.Css.Prelude ( ($), Semigroup((<>)) )


injectClass :: MisoString -> View model action -> View model action
injectClass cn = \case
  VNode ns tg atrs children -> VNode ns tg (injClass atrs) children
  o -> o
  where
    injClass = \case
      [] -> [ClassList [cn]]
      ClassList cls : atrs' -> ClassList (cn : cls) : atrs'
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

eToView :: E en es ei knownIds cls eacs -> View model action
eToView = \case
  CDataE txt -> vtext txt
  NilE enp -> nodeHtml (ms $ symbolVal enp) [] []
  IdE eni e -> injectElementId (ms $ symbolVal eni) (eToView e)
  AppClsE orCls e -> injectClass (className orCls) (eToView e)
  AppendChildE ce pe -> appChild (Tagged @Child (eToView ce)) (eToView pe)

toView :: E en es ei knownIds cls '[] -> View model action
toView = eToView
