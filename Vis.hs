import Fountain
import Graphics.Chalkboard.Array
import Graphics.Chalkboard.Board
import Graphics.Chalkboard.Shapes
import Graphics.Chalkboard.Viewer
import Graphics.Chalkboard.Color
import Control.Concurrent.MVar

main = do
  let config = [WindowSize 400 200
               , WindowPos 10 10
               , PixelSize 2]
      board = fmap (\ x -> if x then green else white) 
              $ rotate 0.05 
              $ scale 0.05
              $ checker 
  m <- newMVar board
  initBoardViewer config m