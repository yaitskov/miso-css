{-# LANGUAGE FunctionalDependencies #-}
module Miso.Css.Event where

import Miso
import Miso.Event as E
import Miso.Css.Prelude

class EventFactory ef m a | ef -> m a where
  eventName :: ef -> MisoString
  mkActionAttribute :: ef -> Attribute m a

data AtomicEvent m a = AtomicEvent MisoString a deriving (Show, Eq, Ord)

instance EventFactory (AtomicEvent m a) m a where
  eventName (AtomicEvent e _) = e
  mkActionAttribute (AtomicEvent e a) =
    E.on e emptyDecoder $ \() _ _ -> a

data VarEvent m a = forall r. VarEvent MisoString (Decoder r) (r -> m -> DOMRef -> a)

instance EventFactory (VarEvent m a) m a where
  eventName (VarEvent en _ _) = en
  mkActionAttribute (VarEvent en decoder af) =
    on en decoder af

onClick :: forall m a. a -> AtomicEvent m a
onClick = AtomicEvent "click"

onVar :: Decoder r -> MisoString -> (r -> a) -> VarEvent m a
onVar d n f = VarEvent n d (\a _ _ -> f a)

onInput :: forall m a. (MisoString -> a) -> VarEvent m a
onInput = onVar valueDecoder "input"

onChange :: forall m a. (MisoString -> a) -> VarEvent m a
onChange = onVar valueDecoder "change"

onKeyPress :: forall m a. (KeyCode -> a) -> VarEvent m a
onKeyPress = onVar keycodeDecoder "keypress"

onKeyUp :: (KeyCode -> a) -> VarEvent m a
onKeyUp = onVar keycodeDecoder "keyup"
