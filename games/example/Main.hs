--import Control.Monad
--import Control.Applicative
import Data.IORef
import Haste
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas

ballRadius :: Double
ballRadius = 20

colRed :: Color
colRed = RGB 255 0 0

colText :: Color
colText = RGB 0 0 0

drawBall :: String -> Point -> Picture ()
drawBall s pt = do
    color colRed  $ fill $ circle pt ballRadius
    color colText $ font "20px Bitstream Vera" $ text pt s

main :: IO ()
main = do
  Just msg <- elemById "msg"
  Just can <- getCanvasById "canvas"
  ptRef <- newIORef $ (0, 0)
  can `onEvent` MouseMove $ \(MouseData (x, y) _ _) -> do
    writeIORef ptRef (convert x, convert y)
  can `onEvent` TouchMove $ \(TouchData _ (e:_) _) -> do
    --setProp msg "innerHTML" $ (toString $ show $ clientCoords $ head es)
    writeIORef ptRef (convert $ fst $ clientCoords e, convert  $ snd $ clientCoords e)
  animate msg can 0 ptRef

animate :: Elem -> Canvas -> Double -> IORef Point -> IO ()
animate msg can angle ptRef = do
  pt <- readIORef ptRef
  render can $ do
    translate (160, 160) $ rotate angle $ do
      translate (100, 100) . rotate (-angle) $ drawBall "★" (0,0)
    drawBall "♥" pt
  --setProp msg "innerHTML" $ (toString $ fst pt)
  setTimer (Once 30) $ animate msg can (angle + 0.03) ptRef
  return ()


