{-# LANGUAGE RankNTypes #-}

--import Control.Monad
--import Control.Applicative
import Control.Arrow
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Foldable
import Data.IORef
import Data.Maybe
import Haste
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Soroban
import Util

-- constant
-------------
pieceRadius :: Double
pieceRadius = 19.5

pieceGap :: Double
pieceGap = 0.5

pieceSpace :: Double
pieceSpace = 2 * (pieceGap + pieceRadius)

colBlack :: Color
colBlack = RGB 0 0 0

colRed :: Color
colRed = RGB 200 0 0

colBlue :: Color
colBlue = RGB 200 0 0

offsetSoroban :: Point
offsetSoroban = (60, 100)

msecPerFrame :: Int
msecPerFrame = 5

initState :: Soroban
initState = blockToSoroban 20 [(0,10)]

-- haste util
----------------
onEventButton :: (IsElem el, MonadEvent m) => el -> MouseEvent -> MouseButton -> ((Int, Int) -> m ()) -> m HandlerInfo
onEventButton el ev bt h = el `onEvent` ev $ \(MouseData p (Just b) _) -> when (b == bt) $ h p

onEventMove :: (IsElem el, MonadEvent m) => el -> MouseEvent -> ((Int, Int) -> m ()) -> m HandlerInfo
onEventMove el ev h = el `onEvent` ev $ \(MouseData p _ _) ->  h p

onEventTouch :: (IsElem el, MonadEvent m) => el -> TouchEvent -> ((Int, Int) -> m ()) -> m HandlerInfo
onEventTouch el ev h = el `onEvent` ev $ \(TouchData _ (e:_) _) -> h $ clientCoords e

cv :: (JSNum a, JSNum b) => (a, a) -> (b, b)
cv = convert *** convert

-- app
----------------
type FocusPoint = Maybe Point
type World m a = ReaderT (Canvas, Elem, IORef FocusPoint, IORef Soroban, IORef String) m a

runWorld :: Canvas -> Elem -> IORef FocusPoint -> IORef Soroban -> IORef String -> World m a -> m a
runWorld c e f s d m = runReaderT m (c, e, f, s, d)

screenToClient :: Point -> Point
screenToClient (x, y) = (x -) *** (y -) $ offsetSoroban

clientToScreenPicture :: Picture () -> Picture ()
clientToScreenPicture = translate (cv offsetSoroban)

checkPosition :: Position -> Maybe Position
checkPosition x = toMaybe (0 <= x && x < numPiece initState) x

registMouseEventHandler :: World IO ()
registMouseEventHandler = do
  (can, msg, ptRef, stRef, debugRef) <- ask
  let renderAllIO = runWorld can msg ptRef stRef debugRef renderAll
  liftIO $ do
    let reflesh' pt = do { writeIORef ptRef pt; renderAllIO;}
    onEventButton can MouseDown MouseLeft $ reflesh' . Just . screenToClient . cv
    onEventButton can MouseUp   MouseLeft $ const $ reflesh' Nothing
    onEventMove   can MouseOut  $ const $ reflesh' Nothing
    onEventMove   can MouseMove $ \p -> do
      fpt <- readIORef ptRef
      fromMaybe noop $ do
        pre <- fpt
        let cur = screenToClient $ cv p
        curPos <- pointToPos cur
        prePos <- pointToPos pre
        return $ do
          st <- readIORef stRef
          whenJust $ do
            st' <- move st prePos curPos
            return $ do
              writeIORef stRef st'
              writeIORef ptRef $ Just cur
  return ()

registTouchEventHandler :: World IO ()
registTouchEventHandler = do
  (can, msg, ptRef, stRef, debugRef) <- ask
  let renderAllIO = runWorld can msg ptRef stRef debugRef renderAll
  liftIO $ do
    let reflesh' pt = do { writeIORef ptRef pt; renderAllIO;}
    onEventTouch can TouchStart $ reflesh' . Just . screenToClient . cv
    onEventTouch can TouchMove  $ \p -> do
      fpt <- readIORef ptRef
      fromMaybe noop $ do
        pre <- fpt
        let cur = screenToClient $ cv p
        curPos <- pointToPos cur
        prePos <- pointToPos pre
        return $ do
          st <- readIORef stRef
          whenJust $ do
            st' <- move st prePos curPos
            return $ do
              writeIORef stRef st'
              writeIORef ptRef $ Just cur
    onEventTouch can TouchEnd   $ const $ reflesh' Nothing
  return ()

animate :: World IO ()
animate = do
  renderAll
  x <- extract' animate
  liftIO $ setTimer (Once msecPerFrame) x
  return ()

renderAll :: World IO ()
renderAll = do
  (can, msg, ptRef, stRef, _) <- ask
  liftIO $ do
    pt <- readIORef ptRef
    st <- readIORef stRef
    --s  <- readIORef debugRef
    render can $ clientToScreenPicture $ drawSoroban pt st
    --setProp msg "innerHTML" $  show $ sorobanToBlock st
drawBall :: Color -> Position -> Picture ()
drawBall c x = color c  $ fill $ circle (posToPoint x)  pieceRadius

drawBlock :: Block -> Picture ()
drawBlock (pos, num) = color colBlue $ font (show (40+num) ++ "px Bitstream Vera") $ text ((xl+xr)/2, y-1.2*pieceRadius) $ show num
  where
    (xl, y) = posToPoint pos
    (xr, _) = posToPoint (pos+num-1) -- dasai

drawFocus :: Maybe Position -> Picture ()
drawFocus x = whenJust $ drawBall colRed <$> x

pointToPos :: Point -> Maybe Position
pointToPos (x, _) = checkPosition (floor $ x / pieceSpace)

posToPoint :: Position -> Point
posToPoint i = cv (pieceSpace * convert i + pieceGap + pieceRadius, pieceRadius)

drawSoroban :: FocusPoint -> Soroban -> Picture ()
drawSoroban fpt s@(Soroban n ps) = do
    drawLine $ convert n
    traverse (drawBall colBlack) ps
    drawFocus $ find (`elem` ps) $ fpt >>= pointToPos
    traverse drawBlock $ sorobanToBlock s
    return ()
    where
        drawLine k = color colBlack $ stroke $ line (cv (0, pieceRadius)) $ cv (k * pieceSpace, pieceRadius)

main :: IO ()
main = do
  stRef <- newIORef initState
  ptRef <- newIORef Nothing
  debugRef <- newIORef ""
  Just msg <- elemById "msg"
  Just can <- getCanvasById "canvas"
  runWorld can msg ptRef stRef debugRef $ do
    registMouseEventHandler
    registTouchEventHandler
    animate
  return ()

