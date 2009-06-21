import Fountain
import qualified Vector as V
import Particle
import Graphics.Chalkboard
import Graphics.Chalkboard.Viewer
import Control.Concurrent.MVar
import Control.Concurrent

main = do
  (f:fs) <- run
  let config = [WindowSize 400 200
               , WindowPos 10 10
               , PixelSize 2
               , FrameTarget 30]
      board frame =          
          let firework :: Particle -> Board Bool
              firework (Particle p v _) = 
                  let (x, y) = (V.vecX p, V.vecY p)
                      (v1, v2) = (V.vecX v + x, V.vecY v + y)
                      angle = angleOfLine ((x, y), (v1, v2))
                  in circularMaskFor (x, y) 0.3 `over`
                     straightline ((x, y), (v1, v2)) 0.05 `over`
                     arrowhead (v1, v2) angle 0.3
              visible c t = if t then alpha c else transparent black
          in fmap unAlpha $ (fmap (visible red)
             $ scale 0.2
             $ move (0, -0.5 / 0.2)
             $ stack (map firework frame)) 
          `over`
             (fmap (visible white)
             $ move (0, -0.5)
             $ straightline ((-10, 0), (10, 0)) 0.01)
          `over`
             (fmap (visible white)
             $ straightline ((0, 10), (0, -10)) 0.01)
  m <- newMVar (board f)
  let loop (f:fs) = do
         putMVar m (board f)
         loop fs
  forkIO (loop fs)
  initBoardViewer config m