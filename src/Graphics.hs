
module Graphics (
  Shape (Square),
  setupGraphics,
  runGraphics,
  renderBoard)
  
  where

import Control.Monad
import Control.Monad.Fix

import Data.IORef

import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT

import Reactive.Banana --((<*>), (<$>))

data Shape = Square (GL.GLfloat,GL.GLfloat,GL.GLfloat)

fire = snd

setupGraphics etimer = do
  (progname, args) <- GLUT.getArgsAndInitialize
  --GLUT.initialWindowMode $ [GLUT.DoubleBuffer, GLUT.RGBA, GLUT.Depth]
  windoe <- GLUT.createWindow "Tetris mark 1" -- 500 500
  GLUT.displayCallback $= (render [])
  GLUT.reshapeCallback $= Just reshape
  GLUT.addTimerCallback 1000 $ loopTimer etimer 

loopTimer event = do
  fire event ()
  GLUT.addTimerCallback 1000 $ loopTimer event

runGraphics = GLUT.mainLoop

renderBoard square = render [square]

reshape (GL.Size xsize ysize) =
  let ratio = (/) (fromIntegral xsize) (fromIntegral ysize) in
   do
     GL.matrixMode $= GL.Projection
     GL.loadIdentity
     GL.viewport $= (GL.Position 0 0, GL.Size xsize ysize)
     --GL.perspective 45 (fromIntegral ratio) 1 1000
     --GL.ortho2D 0 0 (fromIntegral xsize) (fromIntegral ysize)
     GL.matrixMode $= GL.Modelview 0
     GL.loadIdentity
     print "Change Shape!"
     GLUT.postRedisplay Nothing

renderSquare (Square (x, y, width)) = 
  let (x1,y1) = (x,y)
      (x2,y2) = (x, y + width)
      (x3,y3) = (x+width, y+width)
      (x4,y4) = (x+width, y)
      in
   GL.renderPrimitive GL.Quads $ do
     GL.vertex $ (GL.Vertex2 x1 y1 :: GL.Vertex2 GL.GLfloat)
     GL.vertex $ (GL.Vertex2 x2 y2 :: GL.Vertex2 GL.GLfloat)
     GL.vertex $ (GL.Vertex2 x3 y3 :: GL.Vertex2 GL.GLfloat)
     GL.vertex $ (GL.Vertex2 x4 y4 :: GL.Vertex2 GL.GLfloat)
    

render squares = do
  GL.clearColor $= GL.Color4 1 1 1 1
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  (_, GL.Size xres yres) <- GL.get GL.viewport
  GL.ortho2D 0 0 (fromIntegral xres) (fromIntegral yres)
  
  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity
  
  GL.color (GL.Color4 1 0 1 1 :: GL.Color4 GL.GLfloat)
  mapM_ renderSquare squares
  GL.flush
    --GLUT.swapBuffers
  