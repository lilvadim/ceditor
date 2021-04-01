{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module Modifiers 
    ( modifiersList,
      whiteBG,
      negative,
      grayscale,
      gamma,
      noRed,
      noGreen,
      noBlue,
      onlyRed,
      onlyGreen,
      onlyBlue,
      rotate,
      crop,
      scale,
      blur,
      sharpen,
      emboss,
      brush,
      median
    ) where 

import Codec.Picture
import Codec.Picture.Types 
import Control.Monad
import Control.Monad.ST
import Conversion 
import Data.Sort 

-- Helper Functions and Bindings

modifiersList = "Modifiers List (to apply add the argument --do:MODIFIER-CODE)\n" ++
               "Filters:\n" ++
               " neg - Negative (Inversion)\n" ++ 
               " grayscale - Grayscale\n" ++ 
               " gamma-N - Gamma Correction (N is your coefficient)\n" ++
               " no-red / no-green / no-blue - Disable Red/Green/Blue Channel\n" ++
               " only-red / only-green / only-blue - Only Red/Green/Blue Channel\n" ++
               " brush - Aquapaint Brushy Effect\n" ++ 
               " emboss - Embossing\n" ++
               " sharpen - Increase Sharpness\n" ++
               " blur - Gaussian Blur\n" ++ 
               " median - Median Values Filter\n" ++
               "Transformation & Rotation:\n" ++
               " rotate-N - N Degrees Rotation (negative and float numbers supported)\n" ++
               " crop-N - Crop image to N%, resolution also be changing (but minimal quality loss)\n" ++
               " zoom-N - Crop image to N% without changing resolution (50% -> 2X zoom effect, 75% -> cut off 25% of image from each side, N > 100% -> white frame)\n" ++
               " scale-N - Scale the image (resolution) to N% (possible quality loss)"

rounding :: (RealFrac a, Integral b) => a -> b
rounding a = floor (a + 0.5)

pxify = rounding

getX = fst 
getY = snd

toRad deg = deg * (pi/180)

pxPlus :: PixelRGBF -> PixelRGBF -> PixelRGBF
pxPlus (PixelRGBF r1 g1 b1) (PixelRGBF r2 g2 b2) = PixelRGBF (r1 + r2) (g1 + g2) (b1 + b2)

pxMult :: PixelRGBF -> PixelRGBF -> PixelRGBF 
pxMult (PixelRGBF r1 g1 b1) (PixelRGBF r2 g2 b2) = PixelRGBF (r1 * r2) (g1 * g2) (b1 * b2)

pxMultNum :: PixelRGBF -> Float -> PixelRGBF 
pxMultNum (PixelRGBF r g b) q = PixelRGBF (r * q) (g * q) (b * q)

normalizePixel :: PixelRGBF -> PixelRGB8 
normalizePixel (PixelRGBF r g b) = PixelRGB8 (n r) (n g) (n b)
  where n f | f >= 1.0 = 255
            | f <= 0 = 0
            | otherwise = floor $ 255 * f

whiteBG :: Image PixelRGBA8 -> Image PixelRGBA8
whiteBG = pixelMap $ \px@(PixelRGBA8 r g b a) -> if 
                                                  | a == 0 -> whitePx
                                                  | a < 255 -> PixelRGBA8 r g b 255
                                                  | otherwise -> px

getR :: PixelRGBA8 -> Pixel8
getR (PixelRGBA8 r g b a) = r

getG :: PixelRGBA8 -> Pixel8
getG (PixelRGBA8 r g b a) = g

getB :: PixelRGBA8 -> Pixel8
getB (PixelRGBA8 r g b a) = b

getRf :: PixelRGBF -> PixelF
getRf (PixelRGBF r g b) = r

getGf :: PixelRGBF -> PixelF
getGf (PixelRGBF r g b) = g

getBf :: PixelRGBF -> PixelF
getBf (PixelRGBF r g b) = b

whitePx :: PixelRGBA8
whitePx = PixelRGBA8 255 255 255 255

blackPx :: PixelRGBA8 
blackPx = PixelRGBA8 0 0 0 255

-- Per Pixel Filters

negative :: Image PixelRGBA8 -> Image PixelRGBA8
negative = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 (255 - r) (255 - g) (255 - b) a

grayscale :: Image PixelRGBA8 -> Image PixelRGBA8
grayscale = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 (y r g b) (y r g b) (y r g b) a 
  where y r g b = pxify $ fromIntegral r * 0.2126 + fromIntegral g * 0.7152 + fromIntegral b * 0.0722

gamma :: Double -> Image PixelRGBA8 -> Image PixelRGBA8
gamma n = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 (y r) (y g) (y b) a
  where y p = pxify $ ((fromIntegral p / 255) ** (1 / n)) * 255

-- RGB Channels Filters / халтура)

noRed :: Image PixelRGBA8 -> Image PixelRGBA8
noRed = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 0 g b a

noGreen :: Image PixelRGBA8 -> Image PixelRGBA8
noGreen = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 r 0 b a

noBlue :: Image PixelRGBA8 -> Image PixelRGBA8
noBlue = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 r g 0 a

onlyRed :: Image PixelRGBA8 -> Image PixelRGBA8
onlyRed = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 r 0 0 a

onlyGreen :: Image PixelRGBA8 -> Image PixelRGBA8
onlyGreen = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 0 g 0 a

onlyBlue :: Image PixelRGBA8 -> Image PixelRGBA8
onlyBlue = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 0 0 b a

-- Transfromation 

rotate :: Double -> Image PixelRGBA8 -> Image PixelRGBA8
rotate n img@Image {..} = generateImage rotater newW newH
  where rotater x y = if srcX x y < imageWidth && srcX x y >= 0 && srcY x y < imageHeight && srcY x y >= 0
                       then pixelAt img (srcX x y) (srcY x y)
                       else whitePx
        srcX x y = getX center + rounding (fromIntegral (x - getX newCenter) * cos' + fromIntegral (y - getY newCenter) * sin')
        srcY x y = getY center + rounding (fromIntegral (y - getY newCenter) * cos' - fromIntegral (x - getX newCenter) * sin')
        center = (imageWidth `div` 2, imageHeight `div` 2)
        newCenter = (newW `div` 2, newH `div` 2)
        newW = rounding $ abs (fromIntegral imageHeight * sin') + abs (fromIntegral imageWidth * cos')
        newH = rounding $ abs (fromIntegral imageHeight * cos') + abs (fromIntegral imageWidth * sin')
        sin' = sin $ toRad n
        cos' = cos $ toRad n

crop :: Double -> Image PixelRGBA8 -> Image PixelRGBA8
crop n img@Image {..} = generateImage cropper newW newH
  where cropper x y = if srcX x < imageWidth && srcX x >= 0 && srcY y < imageHeight && srcY y >= 0
                       then pixelAt img (srcX x) (srcY y)
                       else whitePx
        newW = rounding $ fromIntegral imageWidth * (n/100)
        newH = rounding $ fromIntegral imageHeight * (n/100)
        srcX x = x + getX origin
        srcY y = y + getY origin
        origin = (rounding (fromIntegral imageWidth * (1/2 - n/200)),
                   rounding (fromIntegral imageHeight * (1/2 - n/200)))

scale :: Double -> Image PixelRGBA8 -> Image PixelRGBA8
scale n img@Image {..} = generateImage scaler newW newH 
  where scaler x y = if srcX x < imageWidth && srcX x >= 0 && srcY y < imageHeight && srcY y >= 0
                       then pixelAt img (srcX x) (srcY y)
                       else whitePx
        newW = rounding $ fromIntegral imageWidth * ratio
        newH = rounding $ fromIntegral imageHeight * ratio
        srcX x = floor $ fromIntegral x * (1 / ratio)
        srcY y = floor $ fromIntegral y * (1 / ratio)
        ratio = n/100

-- Matriсes Convolution Based Filters

blur :: Image PixelRGBA8 -> Image PixelRGBA8 
blur img@Image {..} = promoteImage $ generateImage blurrer imageWidth imageHeight
       where blurrer x y | x >= (imageWidth - offset) || x < offset
                          || y >= (imageHeight - offset) || y < offset = PixelRGB8 255 255 255
                         | otherwise = do
                let applyKernel i j p | j >= matrixLength = applyKernel (i + 1) 0 p
                                      | i >= matrixLength = normalizePixel p 
                                      | otherwise = do 
                                         let outPixel = pxMultNum
                                                          (promotePixel $ dropTransparency $ pixelAt img (x + j - offset) (y + i - offset))
                                                           (kernel !! i !! j)
                                         applyKernel i (j+1) (outPixel `pxPlus` p)
                applyKernel 0 0 (PixelRGBF 0 0 0)
             kernel = [[1  / 255, 4  / 255,  6 / 255,  4 / 255, 1 / 255],
                       [4  / 255, 16 / 255, 24 / 255, 16 / 255, 4 / 255],
                       [6  / 255, 24 / 255, 35 / 255, 24 / 255, 6 / 255],
                       [4  / 255, 16 / 255, 24 / 255, 16 / 255, 4 / 255],
                       [1  / 255, 4  / 255,  6 / 255,  4 / 255, 1 / 255]] 
             matrixLength = length kernel 
             offset = matrixLength `div` 2

sharpen :: Image PixelRGBA8 -> Image PixelRGBA8 
sharpen img@Image {..} = promoteImage $ generateImage sharpener imageWidth imageHeight
       where sharpener x y | x >= (imageWidth - offset) || x < offset
                            || y >= (imageHeight - offset) || y < offset = PixelRGB8 255 255 255
                           | otherwise = do
                let applyKernel i j p | j >= matrixLength = applyKernel (i + 1) 0 p
                                      | i >= matrixLength = normalizePixel p 
                                      | otherwise = do 
                                         let outPixel = pxMultNum
                                                          (promotePixel $ dropTransparency $ pixelAt img (x + j - offset) (y + i - offset))
                                                           (kernel !! i !! j)
                                         applyKernel i (j+1) (pxPlus outPixel p)
                applyKernel 0 0 (PixelRGBF 0 0 0)
             kernel = [[ 0, -1,  0],
                       [-1,  5, -1],
                       [ 0, -1,  0]]
             matrixLength = length kernel
             offset = matrixLength `div` 2
             
emboss :: Image PixelRGBA8 -> Image PixelRGBA8 
emboss img@Image {..} = promoteImage $ generateImage sharpener imageWidth imageHeight
       where sharpener x y | x >= (imageWidth - offset) || x < offset
                            || y >= (imageHeight - offset) || y < offset = PixelRGB8 255 255 255
                           | otherwise = do
                let applyKernel i j p | j >= matrixLength = applyKernel (i + 1) 0 p
                                      | i >= matrixLength = normalizePixel $ p `pxPlus` PixelRGBF 0.5 0.5 0.5
                                      | otherwise = do 
                                         let outPixel = pxMultNum
                                                          (promotePixel $ dropTransparency $ pixelAt img (x + j - offset) (y + i - offset))
                                                           (kernel !! i !! j)
                                         applyKernel i (j+1) (pxPlus outPixel p)
                applyKernel 0 0 (PixelRGBF 0 0 0)
             kernel = [[ 0,  1, 0],
                       [-1,  0, 1],
                       [ 0, -1, 0]]
             matrixLength = length kernel
             offset = matrixLength `div` 2

brush :: Image PixelRGBA8 -> Image PixelRGBA8 
brush img@Image {..} = promoteImage $ generateImage brusher imageWidth imageHeight
       where brusher x y | x >= (imageWidth - offset) || x < offset
                            || y >= (imageHeight - offset) || y < offset = PixelRGB8 255 255 255
                           | otherwise = do
                let pixelList i j ps | j >= matrixLength = pixelList (i + 1) 0 ps
                                     | i >= matrixLength = ps
                                     | otherwise = pixelList i (j+1) $
                                              pixelAt img 
                                                (x + j - offset) 
                                                (y + i - offset):ps
                let pxList = pixelList 0 0 []
                let medianValue = sort pxList !! ceiling (fromIntegral (matrixLength * matrixLength) / 2)
                let applyKernel i j p | j >= matrixLength = applyKernel (i + 1) 0 p
                                      | i >= matrixLength = normalizePixel $ p `pxPlus` PixelRGBF 0.5 0.5 0.5
                                      | otherwise = do 
                                         let outPixel = pxMultNum
                                                          (promotePixel $ dropTransparency $ pixelAt img (x + j - offset) (y + i - offset))
                                                           (kernel !! i !! j)
                                         applyKernel i (j+1) (pxPlus outPixel p)
                applyKernel 0 0 (promotePixel $ dropTransparency medianValue)
             kernel = [[ 0,-1, 0],
                       [-1, 5,-1],
                       [ 0,-1, 0]]
             matrixLength = length kernel
             offset = matrixLength `div` 2

median :: Image PixelRGBA8 -> Image PixelRGBA8 
median img@Image {..} = promoteImage $ generateImage gen imageWidth imageHeight
       where gen x y | x >= (imageWidth - offset) || x < offset
                      || y >= (imageHeight - offset) || y < offset = PixelRGB8 255 255 255
                     | otherwise = do
                let pixelList i j ps | j >= matrixLength = pixelList (i + 1) 0 ps
                                     | i >= matrixLength = ps
                                     | otherwise = pixelList i (j+1) $
                                              pixelAt img 
                                                (x + j - offset) 
                                                (y + i - offset):ps
                let pxList = pixelList 0 0 []
                dropTransparency $ sort pxList !! ceiling (fromIntegral (matrixLength * matrixLength) / 2)
             matrixLength = 3
             offset = matrixLength `div` 2


{-

  - What is the Matrix?

-}