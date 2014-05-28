module ReactHelp (
  EventSource,
  addHandler,
  fire
  ) where

import Reactive.Banana 
  (Moment, 
   Behavior,
   Event,
   compile, 
   mapAccum,
   union,
   (<*>), 
   (<$>), 
   (<$))
import Reactive.Banana.Frameworks 
  (AddHandler, 
   newAddHandler, 
   fromAddHandler, 
   reactimate, 
   actuate, 
   Frameworks)


type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd
