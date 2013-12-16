-- FPS walk along the structure

import Graphics.UI.GLUT
import Data.IORef
import Data.List
import System.Environment
import System.Exit
import Control.Monad
import Data.Bits hiding (rotate)

import Sphere3

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
  hcenter <-  newIORef (0::Int)
  zoom <- newIORef (2::GLfloat)

  displayCallback $= display1 time origin track hcenter zoom
  reshapeCallback $= Just reshape
  idleCallback $= Just (movetime time)
  keyboardMouseCallback $=  Just (keyboardMouse origin track hcenter zoom )
  mainLoop

--- end main


toGLf :: Double -> GLfloat
toGLf x = realToFrac x

movetime time = do tm <- readIORef time
                   writeIORef time (tm+0.015)
                   postRedisplay Nothing                    
 

kula0 =  [(0,0,0)]++ [(sin (t*3.14/180.0),cos (t*3.14/180.0),0) | t <- [0..360]]
--kula scale = map (\(a,b,c) -> (toGLf a,toGLf b,toGLf c)) $ map (\(x,y,z) -> (scale*x,scale*y,scale*z)) kula0
kula scale = map (\(x,y,z) -> (scale*x,scale*y,scale*z)) kula_0
--kula scale =  kula_0
slonce = kula 0.07
ziemia = kula 0.05
mars = kula 0.03
wenus = kula 0.02


-- ( rozmiar, okres obiegu, odleglosc, kolor, nazwa )
planety = [ (0.07,1e12,0.0,(Color3 (1.0::GLfloat) 1.0 0.9),"slonce"),
            (0.03,0.2408,0.3871,(Color3 (0.9::GLfloat) 0.5 0.5),"merkury"),
            (0.03,0.6152,0.7233,(Color3 (0.6::GLfloat) 0.6 0.6),"wenus"),
            (0.03,1.0,1.0,(Color3 (0.1::GLfloat) 0.5 1.0),"ziemia"),
            (0.03,1.8808,1.5237,(Color3 (1.0::GLfloat) 0.2 0.3),"mars"),
            (0.06,11.86,5.2,(Color3 (1.0::GLfloat) 1.0 0.3),"jowisz"),
            (0.05,29.44,9.53,(Color3 (1.0::GLfloat) 1.0 0.1),"saturn"),
            (0.04,84.07,19.19,(Color3 (0.5::GLfloat) 0.9 0.0),"uran"),
            (0.04,164.87,30.06,(Color3 (0.2::GLfloat) 0.2 1.0),"neptun")];


display1 time origin track hcenter zoom= do

-- get state variables         
  tm <-  get time
  org <- get origin   
  trk <- get track
  scl <- get zoom
  helio <- get hcenter

  clear [ ColorBuffer, DepthBuffer ]

  normalize $= Enabled

  depthFunc $= Just Lequal
  lighting $= Enabled
  lightModelLocalViewer  $= Enabled
--  shadeModel $= Smooth
  shadeModel $= Flat

  let l0 = Light 0
  let l1 = Light 0
  

  diffuse  l0 $= ((Color4 (1.0::GLfloat) 1.0 (1.0) 1.0))
  specular l0 $= ((Color4 (0.0::GLfloat) 0.0 (0.0) 1.0))
  ambient  l0 $= ((Color4 (0.0::GLfloat) 0.0 (0.0) 1.0))
  light l0 $= Enabled
  lightModelLocalViewer $= Enabled
  lightModelTwoSide $= Disabled

  diffuse  l1 $= ((Color4 (0.5::GLfloat) 0.5 (0.5) 1.0))
  specular l1 $= ((Color4 (0.0::GLfloat) 0.0 (0.0) 1.0))
  ambient  l1 $= ((Color4 (0.0::GLfloat) 0.0 (0.0) 1.0))
  light l1 $= Enabled

--
  matrixMode $= Projection
  loadIdentity
  frustum (-1) (1)  (-1) (1) (2) 1140   
--

  matrixMode $= Modelview 0
  loadIdentity




  translate (Vector3 (0::GLdouble) 0 (-5))
  if (org .&. 8) /= 0 then do 
     rotate (-65) (Vector3 (1::GLdouble) 0 0 )
  else return()

  scale scl scl (scl)

--  position l0 $= Vertex4 (0.0::GLfloat) (-0.1) (0.2) (1.0)
  let (_,oo,rr,_,_) = planety!!helio
  position l0 $= Vertex4 (toGLf $ (-rr)*(sin (tm/oo))) (toGLf $ (-0.0)+(-rr)*(cos (tm/oo))) (0.0) (1.0)
  position l1 $= Vertex4 (toGLf $ (-rr)*(sin (tm/oo))) (toGLf $ (-0.2)+(-rr)*(cos (tm/oo))) (0.0) (1.0)


--  let helio = if (org .&. 1) /= 0 then 1 else 0
  let hist  = if (org .&. 2) /= 0 then 1 else 0
      earth = if (org .&. 4) /= 0 then 1 else 0

  if (org .&. 2) /= 0 then do 
     track $~ (\x -> tm:x)
  else return ()

  if hist == 1 && earth== 0 then do
    mapM_ (putPlanets helio 0) trk
  else return()


  if hist == 1 && earth== 1 then do
    mapM_ (putPlanetsearth helio 0) trk
  else return()

  if earth == 0 then do 
    putPlanets helio 1 tm 
  else do putPlanetsearth helio 1 tm 

  swapBuffers



putPlanet' 1 tm (size,period,distance,color',name) = do
  if (name == "slonce") then  do  
    materialEmission FrontAndBack   $=  (Color4 1.0 1.0 0.0 1.0) 
    materialDiffuse FrontAndBack    $=  ( col3_to_col4 color')
  else do
    materialEmission FrontAndBack   $=  (Color4 0 0 0 0)
    materialDiffuse FrontAndBack    $=  ( col3_to_col4 color')
    materialAmbient Back   $=  (Color4 0 0 0 0)
    materialSpecular FrontAndBack	 $=  (Color4 0 0 0 0)
  let (a,b,c) = ((toGLf $ distance*(sin (tm/period))),(toGLf $ distance*(cos (tm/period))),0.0)
--  translate  $ Vector3 (toGLf $ distance*(sin (tm/period))) (toGLf $ distance*(cos (tm/period))) 0.0
  renderPrimitive Triangles $  mapM_ (\((x,y,z),(an,bn,cn)) -> do normal$Normal3 x y z 
                                                                  vertex$Vertex3 (x+a) (y+b) (z+c) ) $  (zip (kula size) kula_n)


putPlanet' 0 tm (size,period,distance,color',name) = do
  lighting $= Disabled
  color color'
  let (a,b,c) = ((toGLf $ distance*(sin (tm/period))),(toGLf $ distance*(cos (tm/period))),0.0)
  renderPrimitive Points $  mapM_ (\(x,y,z) -> vertex$Vertex3 (a+x) (b+y) (c+z)     ) $ [(0::GLfloat,0,0)]
  lighting $= Enabled


putPlanets helio style tm = do
           let (_,oo,rr,_,_) = planety!!helio
--           if helio == 1 then do 
           preservingMatrix $ do 
                               translate $ Vector3 (toGLf $ (-rr)*(sin (tm/oo))) (toGLf $ (-rr)*(cos (tm/oo))) 0 
                               mapM_ (putPlanet' style tm) planety
--           else 
--                      mapM_ (putPlanet style tm) planety
           
putPlanetsearth helio style tm = do
           let (_,oo,rr,_,_) = planety!!helio
--           if helio == 1 then do 
           preservingMatrix $ do 
                               translate $ Vector3 (toGLf $ (-rr)*(sin (tm/oo))) (toGLf $ (-rr)*(cos (tm/oo))) 0 
                               mapM_ (putPlanet' style tm) [planety!!0,planety!!3]
           

keyboardMouse  origin track hcenter zoom (Char  'n')  Down c d = do 
       test <- get origin
       let tst = test `xor` 1
       writeIORef origin tst
       postRedisplay Nothing 

keyboardMouse  origin track hcenter zoom (Char  't')  Down c d = do 
       test <- get origin
       let tst = test `xor` 2
       writeIORef origin tst
       writeIORef track []
       postRedisplay Nothing 

keyboardMouse  origin track hcenter zoom  (Char  'z')  Down c d = do 
       test <- get origin
       let tst = test `xor` 4
       writeIORef origin tst
       postRedisplay Nothing 

keyboardMouse  origin track hcenter zoom  (Char  'p')  Down c d = do 
       test <- get origin
       let tst = test `xor` 8
       writeIORef origin tst
       postRedisplay Nothing 




keyboardMouse  origin track hcenter  zoom  (Char  '0')  Down c d = do 
       writeIORef hcenter 0
       postRedisplay Nothing 


keyboardMouse  origin track hcenter  zoom  (Char  '1')  Down c d = do 
       writeIORef hcenter 1
       postRedisplay Nothing 

keyboardMouse  origin track hcenter  zoom  (Char  '2')  Down c d = do 
       writeIORef hcenter 2
       postRedisplay Nothing 

keyboardMouse  origin track hcenter zoom  (Char  '3')  Down c d = do 
       writeIORef hcenter 3
       postRedisplay Nothing 


keyboardMouse  origin track hcenter zoom   (SpecialKey KeyUp)   Down c d = do 
       zoom $~ (\x -> x*(1.3::GLfloat))
       postRedisplay Nothing

keyboardMouse  origin track hcenter zoom   (SpecialKey KeyDown)   Down c d = do 
       zoom $~ (\x -> x/(1.3::GLfloat))
       postRedisplay Nothing


keyboardMouse  origin track hcenter  zoom  (Char  'q')  Down c d =  do exitWith ExitSuccess

keyboardMouse  origin track  _ _ (Char  _ )  _ _ _   = do return ()
keyboardMouse  origin track  _ _ _  _ _ _   = do return ()


reshape s@(Size w h) = do 
  viewport $= (Position 0 0, s)

col3_to_col4 :: Color3 GLfloat -> Color4 GLfloat
col3_to_col4 (Color3 r g b) = Color4 r g b 1.0
