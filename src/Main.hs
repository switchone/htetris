{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. NetworkDescription t"

module Main where

import Data.IORef

import Reactive.Banana 
  (Moment, 
   Behavior,
   Event,
   compile, 
   mapAccum,
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

import Graphics

main = do 
  -- Get some events
  (einit, etimer) <- makeEventSources
  setupGraphics etimer
  network <- compile $ setupNetwork (einit, etimer)
  actuate network
  runGraphics
  
makeEventSources = (,) <$> newAddHandler <*> newAddHandler

type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

applyGravity (Square(x,y,w)) = 
  Square(x,y-w,w)
  
setupNetwork :: forall t. Frameworks t =>
    (EventSource (), EventSource ()) -> Moment t ()
setupNetwork (esinit, estimer) = do
  einit <- fromAddHandler (addHandler esinit)
  etimer <- fromAddHandler (addHandler estimer)
  
  let
    initSquare = Square (0.7, 0.3, 0.3)
    
    esquare :: Event t Shape
    bsquare :: Behavior t Shape
    (esquare, bsquare) = 
      mapAccum initSquare . 
      fmap (\f x -> (f x, f x)) $
      (applyGravity <$ etimer)
                         
  reactimate $ putStrLn "Fired Timer" <$ etimer
  reactimate $ renderBoard <$> esquare
           
