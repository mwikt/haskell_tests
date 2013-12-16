-- FPS walk along the structure

import Graphics.UI.GLUT
import Data.IORef
import Data.List
import System.Environment
import System.Exit
import Control.Monad
import Data.Bits hiding (rotate)

main :: IO ()
main = do 
  (progname,_) <- getArgsAndInitialize
  args <- getArgs
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered, RGBMode ]
  createWindow "Hello World"
--
  time <- newIORef 0.0
  origin <-  newIORef (0::Int)
  track <-  newIORef []

  displayCallback $= display1 time origin track
  reshapeCallback $= Just reshape
  idleCallback $= Just (movetime time)
  keyboardMouseCallback $=  Just (keyboardMouse origin track)
  mainLoop

--- end main


toGLf :: Double -> GLfloat
toGLf x = realToFrac x

movetime time = do tm <- readIORef time
                   writeIORef time (tm+0.025)
                   postRedisplay Nothing                    
 

kula0 =  [(0,0,0)]++ [(sin (t*3.14/180.0),cos (t*3.14/180.0),0) | t <- [0..360]]
kula scale = map (\(a,b,c) -> (toGLf a,toGLf b,toGLf c)) $ map (\(x,y,z) -> (scale*x,scale*y,scale*z)) kula0
slonce = kula 0.07
ziemia = kula 0.05
mars = kula 0.03
wenus = kula 0.02

display1 time origin track = do

-- get state variables         
  tm <-  get time
  org <- get origin   
  trk <- get track
  clear [ ColorBuffer, DepthBuffer ]

  depthFunc $= Nothing


--
  matrixMode $= Projection
  loadIdentity
--  frustum (-0.25+pshiftx) (0.25+pshiftx)  (-0.25+pshifty) (0.25+pshifty) (2) 1140   
--

  matrixMode $= Modelview 0
  loadIdentity
  scale 0.5 0.5 (0.5::GLfloat)

  if (org .&. 1) /= 0 then do 
    translate $ Vector3 (toGLf $ (-0.5)*(sin tm)) (toGLf $ (-0.5)*(cos tm)) 0 
  else return ()

  if (org .&. 2) /= 0 then do 
     track $~ (\x -> tm:x)
  else return ()


  
  color$Color3 (1.0::GLfloat) 1.0 1.0
  preservingMatrix $ do
    renderPrimitive TriangleFan $  mapM_ (\(x,y,z) -> vertex$Vertex3 x y z     ) slonce

  color$Color3 (0.0::GLfloat) 0.0 1.0
  preservingMatrix $ do
    translate $ Vector3 (toGLf $ 0.5*(sin tm)) (toGLf $ 0.5*(cos tm)) 0 
    renderPrimitive TriangleFan $  mapM_ (\(x,y,z) -> vertex$Vertex3 x y z     ) ziemia


  color$Color3 (1.0::GLfloat) 0.0 0.0
  preservingMatrix $ do
    translate $ Vector3 (toGLf $ 0.75*(sin (0.45*tm))) (toGLf $ 0.75*(cos (0.45*tm))) 0  
    renderPrimitive TriangleFan $  mapM_ (\(x,y,z) -> vertex$Vertex3 x y z     ) mars


  color$Color3 (0.0::GLfloat) 1.0 1.0
  preservingMatrix $ do
    translate $ Vector3 (toGLf $ 0.15*(sin (1.45*tm))) (toGLf $ 0.15*(cos (1.45*tm))) 0  
    renderPrimitive TriangleFan $  mapM_ (\(x,y,z) -> vertex$Vertex3 x y z     ) mars


  swapBuffers



keyboardMouse  origin track  (Char  'n')  Down c d = do 
       test <- get origin
       let tst = test `xor` 1
       writeIORef origin tst
       postRedisplay Nothing 

keyboardMouse  origin track  (Char  't')  Down c d = do 
       test <- get origin
       let tst = test `xor` 2
       writeIORef origin tst
       postRedisplay Nothing 


keyboardMouse  origin track  (Char  _ )  _ _  _   = do return ()


reshape s@(Size w h) = do 
  viewport $= (Position 0 0, s)

