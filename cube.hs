import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

n :: [Normal3 GLfloat]
n = [(Normal3 (-1.0) 0.0 0.0),
     (Normal3 0.0 1.0 0.0),
     (Normal3 1.0 0.0 0.0),
     (Normal3 0.0 (-1.0) 0.0),
     (Normal3 0.0 0.0 1.0),
     (Normal3 0.0 0.0 (-1.0))]

faces :: [[Vertex3 GLfloat]]
faces = [[(v 0), (v 1), (v 2), (v 3)],
         [(v 3), (v 2), (v 6), (v 7)],
         [(v 7), (v 6), (v 5), (v 4)],
         [(v 4), (v 5), (v 1), (v 0)],
         [(v 5), (v 6), (v 2), (v 1)],
         [(v 7), (v 4), (v 0), (v 3)]]

v :: Int -> Vertex3 GLfloat
v x = Vertex3 v0 v1 v2
    where v0
              | x == 0 || x == 1 || x == 2 || x == 3 = -1
              | x == 4 || x == 5 || x == 6 || x == 7 = 1
          v1
              | x == 0 || x == 1 || x == 4 || x == 5 = -1
              | x == 2 || x == 3 || x == 6 || x == 7 = 1
          v2
              | x == 0 || x == 3 || x == 4 || x == 7 = 1
              | x == 1 || x == 2 || x == 5 || x == 6 = -1

drawBox :: IO ()
drawBox = let nfaces = zip n faces
          in do mapM (\(n, [v0, v1, v2, v3]) -> do
                        renderPrimitive Quads $ do
                          normal n
                          vertex v0
                          vertex v1
                          vertex v2
                          vertex v3) nfaces
                return ()

display :: DisplayCallback
display = do
  clear [ColorBuffer, DepthBuffer]
  drawBox
  swapBuffers

lightDiffuse :: Color4 GLfloat
lightDiffuse = Color4 1.0 0.0 0.0 1.0

lightPosition :: Vertex4 GLfloat
lightPosition = Vertex4 1.0 1.0 1.0 0.0

initfn :: IO ()
initfn = let light0 = Light 0
         in do diffuse light0 $= lightDiffuse
               position light0 $= lightPosition
               light light0 $= Enabled
               lighting $= Enabled

               depthFunc $= Just Lequal

               matrixMode $= Projection
               perspective 40.0 1.0 1.0 10.0
               matrixMode $= Modelview 0
               lookAt (Vertex3 0.0 0.0 5.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
  
               translate ((Vector3 0.0 0.0 (-1.0))::Vector3 GLfloat)
               rotate 60 ((Vector3 1.0 0.0 0.0)::Vector3 GLfloat)
               rotate (-20) ((Vector3 0.0 0.0 1.0)::Vector3 GLfloat)

main :: IO ()
main = do
  getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
  createWindow "red 3D lighted cube"
  displayCallback $= display
  initfn
  mainLoop