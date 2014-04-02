{- BoardStructure

Defines squares and colors that exist on a grid

Provides mechanism for random board generation
with option to weight the number of blocks to the bottom of the board

[[ Implementation idea: start with large probability of block appearing
(configurable), that decays at a rate (configurable) as you move up the board, 
becoming 0 at a (configurable) stop row. ]]
-}


{- How to structure this?
data
Array
class
-}

module BoardStructure 
       (Color (Clear, Hue),
        BoardSquare (BoardSquare),
        Board (Board),
        constructEmptyBoard,
        constructRandomBoardGen)
       where

import Data.Array
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Gen
import System.Random

data Color = Clear
           | Hue Int
             deriving (Show, Eq, Read)

data BoardSquare = BoardSquare (Bool, Color)
                   deriving Show
                            
data Board = Board (Array (Int, Int) BoardSquare)
           deriving Show
               

constructEmptyBoard w h =
  let indices = ((0,0),(w-1,h-1))
      in
   array indices 
   [(i, (False, Clear)) | i <- range indices]
   
constructEmptyBoardInt :: (Num t, Num t1, Ix t, Ix t1) => t -> t1 -> Array (t, t1) (Bool, Color)
constructEmptyBoardInt = constructEmptyBoard

instance Arbitrary BoardSquare where
  arbitrary = do
    m <- choose (False, True)
    n <- choose (-10,10)
    return $ BoardSquare (m, (\x -> if x <= 0 then Clear else Hue x) n )
    
constructRandomBoardGen w h = 
  let indices = ((0,0),(w-1,h-1))
      in
   do 
     squareList <- (vector (w*h)) :: (Gen [BoardSquare])
     return $ Board $ listArray indices squareList 

instance Random Color where
  random g = 
    let (colorVal, gen) = randomR (0x000000, 0xFFFFFF) g 
    in
    if colorVal `mod` 3 == 0 then
      (Clear, gen)
    else
      (Hue colorVal, gen)
  
  randomR (Clear, Clear) g = (Clear, g)
  randomR (Hue x, Hue y) g = let (colorVal,gen) = randomR (x, y) g in (Hue colorVal, gen)
  randomR (Clear, Hue x) g = let (colorVal,gen) = randomR (-x, x) g in
    if colorVal < 0 then
      (Clear, gen)
    else
      (Hue colorVal, gen)
  randomR (Hue x, Clear) g = (Clear, g)

       
{-instance Random Board where
  random g = zip (random g)::(Bool, RandomGen g) (randomR (-10,10)
-}
  

  
--instance Arbitrary Board where
--  arbitrary = constructRandomBoardGen 1 2

