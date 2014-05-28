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
   apply,
   filterE,
   whenE,
   never,
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
import TShapes
import Data.Array

import ReactHelp

boardHeight = 36
boardWidth = 12

shapeStart = (quot boardWidth 2, boardHeight - 1)

-- TEST Functions -----
createRandomBoard randGen rNum = boardToShapes $ (unGen (constructRandomBoardGen boardWidth boardHeight) randGen rNum)

main = do 
  -- Get some events
  (einit, etimer, eleft, eright, edown) <- makeEventSources
  randGen <- newStdGen
  rNum :: Int <- randomIO
  let initBoard = constructEmptyBoard boardWidth boardHeight
  setupGraphics einit etimer eleft eright edown
  network <- compile $ setupNetwork (einit, etimer, eleft, eright, edown) initBoard
  actuate network
  runGraphics
  
makeEventSources = (,,,,) <$> newAddHandler <*> newAddHandler <*> newAddHandler <*> newAddHandler <*> newAddHandler

--applyGravity (Square(x,y,w,c)) = 
--  Square(x,y-w,w,c)

applyGravity shapes =
  let blockGravity ((x,y), col) = ((x,y-1),col) in
  map blockGravity shapes
  
  
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
   [ Square(locationX x, locationY y, squareWidth, convertColor c) | 
     ((x,y), BoardSquare (use,c)) <- allItems, 
     use ]
  
{- The Network goes like this:
 -  timer updates shape
 -  timer and shape updates grid
 -  input updates shape
 -  shape and grid updates esquare
 -  esquare sends event to GUI
-}

setupNetwork :: forall t. Frameworks t =>
    (EventSource (), EventSource (), EventSource (), EventSource (), EventSource ()) -> Board -> Moment t ()
setupNetwork (esinit, estimer, esleft, esright, esdown) initBoard = do
  einit <- fromAddHandler (addHandler esinit)
  etimer <- fromAddHandler (addHandler estimer)
  eleft <- fromAddHandler (addHandler esleft)
  eright <- fromAddHandler (addHandler esright)
  edown <- fromAddHandler (addHandler esdown)
  {- TODO structure: 
   - eGrid Behavior t Board --background board
   - eShape Behavior t [((Int,Int), Color)]

   - implement a "shapeConflicts" function to see if eShape intersects with eGrid
   - esquare gets based on adding eShape to eGrid and filtering result through boardToShapes
   -}
  let initSquares = boardToShapes initBoard
      
  let
    eBackground :: Event t Board
    bBackground :: Behavior t Board
    (eBackground, bBackground) =
      mapAccum initBoard .
      fmap (\f x -> (f x, f x)) $
      ((\s b -> addListToBoard b s) <$> eshapeplaced)
      
    eShape :: Event t [((Int,Int), Color)]
    bShape :: Behavior t [((Int,Int), Color)]
    (eShape, bShape) =
      mapAccum [((0,5), Hue 0)] .
      fmap (\f x -> (f x, f x)) $
      (applyGravity <$ etimer)
      `union`(id <$ eleft) 
      `union` (id <$ eright) 
      `union` (id <$ edown)
      `union` ((\s -> []) <$ ePlaceShape)
      
    eGrid :: Event t Board   
    eGrid = apply (addListToBoard <$> bBackground) eShape
  
    esquare :: Event t [Shape]
    esquare = fmap boardToShapes eGrid
    
    --eSeparateGrid :: Event t ([((Int,Int), Color)], Board)
    --eSeparateGrid = (,) <$> eShape <*> eBackground
  
    bPlaceShape = isShapePlaced <$> bBackground <*> bShape
    ePlaceShape = () <$ whenE bPlaceShape eShape
    --ePlaceShape = () <$ filterE id $ apply (isShapePlaced <$> bBackground) eShape
    eNewShape = never
    eshapeplaced :: Event t [((Int,Int), Color)]
    eshapeplaced = whenE bPlaceShape eShape
 
  reactimate $ putStrLn "!#$#@!$#" <$ einit
  reactimate $ putStrLn "Going Down" <$ edown  
  reactimate $ putStrLn "Going Left" <$ eleft
  reactimate $ putStrLn "Going Right" <$ eright  
  reactimate $ renderBoard <$> esquare
           
