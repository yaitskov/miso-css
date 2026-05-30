{-# LANGUAGE FunctionalDependencies #-}
module Miso.Css.Event where

import Miso
import Miso.Event as E
import Miso.Css.Prelude

class EventFactory ef a | ef -> a where
  eventName :: ef -> MisoString
  mkActionAttribute :: ef -> Attribute a

data AtomicEvent a = AtomicEvent MisoString a deriving (Show, Eq, Ord)

instance EventFactory (AtomicEvent a) a where
  eventName (AtomicEvent e _) = e
  mkActionAttribute (AtomicEvent e a) =
    E.on e emptyDecoder $ \() _ -> a

data VarEvent a = forall r. VarEvent MisoString (Decoder r) (r -> DOMRef -> a)

instance EventFactory (VarEvent a) a where
  eventName (VarEvent en _ _) = en
  mkActionAttribute (VarEvent en decoder af) =
    on en decoder af

onClick :: a -> AtomicEvent a
onClick = AtomicEvent "click"

onVar :: Decoder r -> MisoString -> (r -> a) -> VarEvent a
onVar d n f = VarEvent n d (\a _ -> f a)

onInput :: (MisoString -> a) -> VarEvent a
onInput = onVar valueDecoder "input"

onChange :: (MisoString -> a) -> VarEvent a
onChange = onVar valueDecoder "change"

onKeyPress :: (KeyCode -> a) -> VarEvent a
onKeyPress = onVar keycodeDecoder "keypress"

onKeyUp :: (KeyCode -> a) -> VarEvent a
onKeyUp = onVar keycodeDecoder "keyup"
