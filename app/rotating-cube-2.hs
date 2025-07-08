import Control.Concurrent (threadDelay)
import Control.Monad (forM_, forever, when)
import Control.Monad.State
import Data.Array.IO
import System.Console.ANSI
import qualified System.Console.Terminal.Size as Term

-- Constants
distanceFromCam :: Double
distanceFromCam = 100.0

k1 :: Double
k1 = 40.0

backgroundAsciiCode :: Char
backgroundAsciiCode = ' '

defaultCubeWidth :: Double
defaultCubeWidth = 20.0

incrementSpeed :: Double
incrementSpeed = 0.6

horizontalOffset :: Double
horizontalOffset = 0.05 * defaultCubeWidth

data CubeState = CubeState
  { buffer :: IOArray Int Char,
    zBuffer :: IOArray Int Double,
    angleA :: Double,
    angleB :: Double,
    angleC :: Double,
    cubeWidth :: Int,
    cubeHeight :: Int
  }

type App = StateT CubeState IO

calculateX :: Double -> Double -> Double -> Double -> Double -> Double -> Double
calculateX i j k a b c =
  j * sin a * sin b * cos c
    - k * cos a * sin b * cos c
    + j * cos a * sin c
    + k * sin a * sin c
    + i * cos b * cos c

calculateY :: Double -> Double -> Double -> Double -> Double -> Double -> Double
calculateY i j k a b c =
  j * cos a * cos c
    + k * sin a * cos c
    - j * sin a * sin b * sin c
    + k * cos a * sin b * sin c
    - i * cos b * sin c

calculateZ :: Double -> Double -> Double -> Double -> Double -> Double
calculateZ i j k a b =
  k * cos a * cos b - j * sin a * cos b + i * sin b

calculateForSurface :: Double -> Double -> Double -> Char -> App ()
calculateForSurface cubeX cubeY cubeZ ch = do
  state1 <- get
  let w = fromIntegral $ cubeWidth state1
      h = fromIntegral $ cubeHeight state1
      a = angleA state1
      b = angleB state1
      c = angleC state1
      x = calculateX cubeX cubeY cubeZ a b c
      y = calculateY cubeX cubeY cubeZ a b c
      z = calculateZ cubeX cubeY cubeZ a b + distanceFromCam
      ooz = 1.0 / z
      xp = w / 2.0 + horizontalOffset + k1 * ooz * x * 2.0
      yp = h / 2.0 + k1 * ooz * y
      idx = floor xp + floor yp * cubeWidth state1

  when (idx >= 0 && idx < cubeWidth state1 * cubeHeight state1) $ do
    zbuf <- liftIO $ readArray (zBuffer state1) idx
    when (ooz > zbuf) $ do
      liftIO $ writeArray (zBuffer state1) idx ooz
      liftIO $ writeArray (buffer state1) idx ch

clearBuffers :: App ()
clearBuffers = do
  state2 <- get
  let size = cubeWidth state2 * cubeHeight state2
  liftIO $ do
    mapM_ (\i -> writeArray (buffer state2) i backgroundAsciiCode) [0 .. size - 1]
    mapM_ (\i -> writeArray (zBuffer state2) i 0.0) [0 .. size - 1]

renderCube :: App ()
renderCube = do
  let loop x y
        | x >= defaultCubeWidth = return ()
        | y >= defaultCubeWidth = loop (x + incrementSpeed) (-defaultCubeWidth)
        | otherwise = do
            mapM_
              (\(cx, cy, cz, ch) -> calculateForSurface cx cy cz ch)
              [ (x, y, -defaultCubeWidth, '@'),
                (defaultCubeWidth, y, x, '$'),
                (-defaultCubeWidth, y, -x, '%'),
                (-x, y, defaultCubeWidth, '#'),
                (x, -defaultCubeWidth, -y, ';'),
                (x, defaultCubeWidth, y, '+')
              ]
            loop x (y + incrementSpeed)
  loop (-defaultCubeWidth) (-defaultCubeWidth)

renderFrame :: App ()
renderFrame = do
  state3 <- get
  liftIO $ do
    setCursorPosition 0 0
    clearScreen
    let w = cubeWidth state3
        h = cubeHeight state3
    forM_ [0 .. h - 1] $ \y -> do
      forM_ [0 .. w - 1] $ \x -> do
        c <- readArray (buffer state3) (x + y * w)
        putChar c
      putStrLn ""
  modify $ \s ->
    s
      { angleA = angleA s + 0.05,
        angleB = angleB s + 0.05,
        angleC = angleC s + 0.05
      }
  liftIO $ threadDelay 40000

mainLoop :: App ()
mainLoop = forever $ do
  clearBuffers
  renderCube
  renderFrame

main :: IO ()
main = do
  size <-
    maybe (80, 24) (\w -> (Term.width w - 2, Term.height w - 2))
      <$> Term.size
  let (w, h) = size
  buf <- newArray (0, w * h - 1) backgroundAsciiCode
  zbuf <- newArray (0, w * h - 1) 0.0
  let initialState = CubeState buf zbuf 0 0 0 w h
  _ <- runStateT mainLoop initialState
  return ()