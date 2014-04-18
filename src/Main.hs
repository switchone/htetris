{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. NetworkDescription t"

module Main where

import Data.IORef

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
  
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Gen
import System.Random

import Graphics
import BoardStructure
import Data.Array

import ReactHelp

boardHeight = 36
boardWidth = 12

main = do 
  -- Get some events
  (einit, etimer, eleft, eright, edown) <- makeEventSources
  randGen <- newStdGen
  rNum :: Int <- randomIO
  let randBoard = boardToShapes $ (unGen (constructRandomBoardGen boardWidth boardHeight) randGen rNum)
  setupGraphics etimer eleft eright edown
  network <- compile $ setupNetwork (einit, etimer, eleft, eright, edown) randBoard
  actuate network
  runGraphics
  
makeEventSources = (,,,,) <$> newAddHandler <*> newAddHandler <*> newAddHandler <*> newAddHandler <*> newAddHandler

applyGravity (Square(x,y,w)) = 
  Square(x,y-w,w)
  
  
boardToShapes :: Board -> [Shape]
boardToShapes (Board board) = 
  let totalWidth = 2
      totalHeight = 2
      allItems = assocs board
      ((lastX,lastY), anything) = last allItems 
      numSquaresWide = fromIntegral $ lastX + 1
      numSquaresTall = fromIntegral $ lastY + 1
      squareHeight = totalHeight / numSquaresTall
      squareWidth = squareHeight -- assumes (correctly) that the height is bigger than the width
      offsetX = -1
      offsetY = -1
      locationX x = (fromIntegral x) * squareWidth + offsetX
      locationY y = (fromIntegral y) * squareHeight + offsetY
  in
   [ Square(locationX x, locationY y, squareWidth) | 
     ((x,y), BoardSquare (use,c)) <- allItems, 
     use ]
  
setupNetwork :: forall t. Frameworks t =>
    (EventSource (), EventSource (), EventSource (), EventSource (), EventSource ()) -> [Shape] -> Moment t ()
setupNetwork (esinit, estimer, esleft, esright, esdown) initSquares = do
  einit <- fromAddHandler (addHandler esinit)
  etimer <- fromAddHandler (addHandler estimer)
  eleft <- fromAddHandler (addHandler esleft)
  eright <- fromAddHandler (addHandler esright)
  edown <- fromAddHandler (addHandler esdown)
  
  let
    esquare :: Event t [Shape]
    bsquare :: Behavior t [Shape]
    (esquare, bsquare) = 
      mapAccum initSquares . 
      fmap (\f x -> (f x, f x)) $
      (id <$ etimer) `union`
      --(id <$ eleft) `union`
      --(id <$ eright) `union`
      --(id <$ edown))
      
                         
  reactimate $ putStrLn "Going Down" <$ edown  
  reactimate $ putStrLn "Going Left" <$ eleft
  reactimate $ putStrLn "Going Right" <$ eright  
  reactimate $ renderBoard <$> esquare
           
