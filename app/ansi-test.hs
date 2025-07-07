{-# LANGUAGE NumericUnderscores #-}

import Control.Monad (when, forM_)  -- Add forM_
import System.Console.ANSI
import Control.Concurrent (threadDelay)
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import Data.Vector (Vector)
import qualified Data.Vector as V

-- Constants
distanceFromCam :: Double
distanceFromCam = 100.0

k1 :: Double
k1 = 40.0

backgroundAsciiCode :: Char
backgroundAsciiCode = ' '

cubeWidth :: Double
cubeWidth = 20.0

incrementSpeed :: Double
incrementSpeed = 0.6

horizontalOffset :: Double
-- horizontalOffset = 0.05 * cubeWidth
horizontalOffset = 0.002 * cubeWidth

-- Pure calculation functions
calculateX :: Double -> Double -> Double -> Double -> Double -> Double -> Double
calculateX i j k a b c =
    j * sin a * sin b * cos c - k * cos a * sin b * cos c +
    j * cos a * sin c + k * sin a * sin c + i * cos b * cos c

calculateY :: Double -> Double -> Double -> Double -> Double -> Double -> Double
calculateY i j k a b c =
    j * cos a * cos c + k * sin a * cos c - j * sin a * sin b * sin c +
    k * cos a * sin b * sin c - i * cos b * sin c

calculateZ :: Double -> Double -> Double -> Double -> Double -> Double
calculateZ i j k a b =
    k * cos a * cos b - j * sin a * cos b + i * sin b

-- Main rendering function
calculateForSurface :: Double -> Double -> Double -> Double -> Double -> Double ->
                      Char -> Int -> Int -> MV.MVector s Double -> MV.MVector s Char -> ST s ()
calculateForSurface cubeX cubeY cubeZ a b c ch width height zBuffer buffer = do
    let x = calculateX cubeX cubeY cubeZ a b c
        y = calculateY cubeX cubeY cubeZ a b c
        z = calculateZ cubeX cubeY cubeZ a b + distanceFromCam
        ooz = 1.0 / z
        xp = fromIntegral width / 2.0 + horizontalOffset + k1 * ooz * x * 2.0
        yp = fromIntegral height / 2.0 + k1 * ooz * y
        idx = floor xp + floor yp * width

    when (idx >= 0 && idx < width * height) $ do
        currentZ <- MV.read zBuffer idx
        when (ooz > currentZ) $ do
            MV.write zBuffer idx ooz
            MV.write buffer idx ch

printBuffer :: Int -> Int -> Vector Char -> IO ()
printBuffer width height buffer = do
    forM_ [0..(height-1)] $ \y -> do
        forM_ [0..(width-1)] $ \x -> do
            putChar (buffer V.! (x + y * width))
        putStrLn ""

renderCube :: Int -> Int -> Double -> Double -> Double -> MV.MVector s Char -> MV.MVector s Double -> ST s ()
renderCube width height a b c buffer zBuffer = do
    cubeLoop (-cubeWidth)
  where
    cubeLoop cubeX =
        if cubeX >= cubeWidth then return ()
        else do
            innerLoop cubeX (-cubeWidth)
            cubeLoop (cubeX + incrementSpeed)

    innerLoop cubeX cubeY =
        if cubeY >= cubeWidth then return ()
        else do
            calculateForSurface cubeX cubeY (-cubeWidth) a b c '@' width height zBuffer buffer
            calculateForSurface cubeWidth cubeY cubeX a b c '$' width height zBuffer buffer
            calculateForSurface (-cubeWidth) cubeY (-cubeX) a b c '%' width height zBuffer buffer
            calculateForSurface (-cubeX) cubeY cubeWidth a b c '#' width height zBuffer buffer
            calculateForSurface cubeX (-cubeWidth) (-cubeY) a b c ';' width height zBuffer buffer
            calculateForSurface cubeX cubeWidth cubeY a b c '+' width height zBuffer buffer
            innerLoop cubeX (cubeY + incrementSpeed)

main :: IO ()
main = do
    hideCursor
    maybeDimensions <- getTerminalSize
    case maybeDimensions of
        Nothing -> putStrLn "Could not get terminal size"
        Just (w, h) -> do
            let width = w
                height = h
                dimensions = width * height
            putStrLn $ "Terminal size: " ++ show width ++ "x" ++ show height
            -- setCursorPosition 0 0
            loop dimensions width height 0.0 0.0 0.0
    showCursor
  where
    loop dimensions width height a b c = do
        let bufferContent = runST $ do
                buffer <- V.thaw $ V.replicate dimensions backgroundAsciiCode
                zBuffer <- V.thaw $ V.replicate dimensions 0.0
                renderCube width height a b c buffer zBuffer
                V.freeze buffer

        putStrLn $ "Buffer size: " ++ show (V.length bufferContent)
        -- setCursorPosition 0 0
        printBuffer width height bufferContent
        -- threadDelay 200_000
        -- loop dimensions width height (a + 0.05) (b + 0.05) (c + 0.05)