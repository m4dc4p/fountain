import Vector (vector2)
import qualified Vector as V
import Particle
import Graphics.Chalkboard
import Graphics.Chalkboard.Viewer
import Control.Concurrent.MVar
import Control.Concurrent
import Data.List (unfoldr)
import System.Random

data FP = FP Position Velocity Damping RGB
             
instance Show FP where
    show = showParticle
  
showParticle (FP pos vel damp _) = 
    "(" ++ show (V.vecX pos) ++ ", " ++ show (V.vecY pos) ++ ") [" ++
    show (V.vecX vel) ++ ", " ++ show (V.vecY vel) ++ 
    "] {" ++ show damp ++ "}"
  
instance Particle FP where
    parPosition (FP pos _ _ _) = pos
    parVelocity (FP _ vel _ _) = vel
    parDamping (FP _ _ d _) = d
    setPosition pos (FP _ v d c) = FP pos v d c
    setVelocity vel (FP p _ d c) = FP p vel d c
    setDamping damp (FP p v _ c) = FP p v damp c
  
type NumParticles = Int
type Elapsed = Float
  
fountain :: WorldState ([Float], Elapsed, NumParticles, Duration, [RGB]) FP -> WorldState ([Float], Elapsed, NumParticles, Duration, [RGB]) FP
fountain ((rs, elapsed, cnt, dur, colors), w) =
        let vis = filter visible w
            rate = 0.2
            amt = 2
            new = if elapsed > rate || null vis
                   then take amt . map newParticle $ zip colors (unfoldr (takes 3) rs)
                   else []
            elapsed' = if elapsed > rate || null vis
                        then 0
                        else elapsed + dur
            takes n rs = Just (take n rs, drop n rs)
            -- Need to refactor this portion out?
            w' = take cnt $ vis ++ new
            visible p = V.vecY (parPosition p) > -1
            newParticle (c, [r1,r2,r3]) = FP (vector2 0 0) 
                                          (vector2 (1 * r1) 
                                                   (10 + 2 * r2))
                                          (0.8 + 0.1 * r3)
                                          c
        in ((drop (length new * 3) rs, elapsed', cnt, dur, drop (length new) colors), w')
  
  

main = do
  r <- getStdGen
  let dur = 1 / 30
      init = ((randomRs (-1, 1) r), 0, 100, 1 / 30, cycle [red, white, blue]) 
  (f:fs) <- return $ simulateWith fountain init dur
  let config = [WindowSize 800 600
               , WindowPos 10 10
               , PixelSize 20
               , FrameTarget 30]
      board frame =          
          let firework (FP p v _ c) = 
                  let (x, y) = (V.vecX p, V.vecY p)
                      (v1, v2) = (V.vecX v + x, V.vecY v + y)
                      angle = angleOfLine ((x, y), (v1, v2))
                  in fmap (visible c) $
                     circularMaskFor (x, y) 0.3 -- `over`
--                      straightline ((x, y), (v1, v2)) 0.05 `over`
--                      arrowhead (v1, v2) angle 0.3
              visible c t = if t then alpha c else transparent black
              fireworks = map firework frame
          in fmap unAlpha $ (stack $ map (\f -> scale 0.2
             $ move (0, -0.5 / 0.2) f) fireworks)
  m <- newMVar (board f)
  let loop (f:fs) = do
         putMVar m (board f)
         loop fs
  forkIO (loop fs)
  initBoardViewer config m