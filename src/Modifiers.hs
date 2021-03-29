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
      scale
    ) where 

import Codec.Picture
import Codec.Picture.Types 
import Control.Monad

-- Helper Functions
modifiersList = "Modifiers List (to apply add the argument --do:MODIFIER-CODE)\n" ++
               "Filters:\n" ++
               " neg - Negative (Inversion)\n" ++ 
               " grayscale - Grayscale\n" ++ 
               " gamma-N - Gamma Correction (N is your coefficient)\n" ++
              -- " aquarel - Aquarelisation\n" ++ 
              -- " emboss - Embossing\n" ++
              -- " sharp - Increase Sharpness\n" ++
              -- " blur - Gaussian Blur\n" ++ 
               " no-red / no-green / no-blue - Disable Red/Green/Blue Channel\n" ++
               " only-red / only-green / only-blue - Only Red/Green/Blue Channel\n" ++
               "Transformation & Rotation:\n" ++
               " rotate-N - N Degrees Rotation (negative and float numbers supported)\n" ++
               " crop-N - Crop image to N% (50% -> 2X zoom effect, 75% -> cut off 25% of image from each side, N > 100% -> white frame)\n"

rounding :: (RealFrac a, Integral b) => a -> b
rounding a = floor (a + 0.5)

pxify = rounding

getX = fst 
getY = snd

toRad deg = deg * (pi/180)

whiteBG :: Image PixelRGBA8 -> Image PixelRGBA8
whiteBG = pixelMap $ \px@(PixelRGBA8 r g b a) -> if 
                                              | a == 0 -> PixelRGBA8 255 255 255 255 
                                              | a < 255 -> PixelRGBA8 r g b 1
                                              | otherwise -> px

-- Per Pixel Filters
negative :: Image PixelRGBA8 -> Image PixelRGBA8
negative = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 (255 - r) (255 - g) (255 - b) a

grayscale :: Image PixelRGBA8 -> Image PixelRGBA8
grayscale = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 (y r g b) (y r g b) (y r g b) a 
  where y r g b = pxify $ fromIntegral r * 0.2126 + fromIntegral g * 0.7152 + fromIntegral b * 0.0722

gamma :: Double -> Image PixelRGBA8 -> Image PixelRGBA8
gamma n = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 (y r) (y g) (y b) a
  where y p = pxify $ ((fromIntegral p / 255) ** (1 / n)) * 255

-- RGB Channels Filters
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
                       else PixelRGBA8 255 255 255 255
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
                       else PixelRGBA8 255 255 255 255
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
                       else PixelRGBA8 255 255 255 255
        newW = rounding $ fromIntegral imageWidth * ratio
        newH = rounding $ fromIntegral imageHeight * ratio
        srcX x = floor $ fromIntegral x * (1 / ratio)
        srcY y = floor $ fromIntegral y * (1 / ratio)
        ratio = 1/ (n/100) 

-- Matriсes Based Filters


{-
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
▀█▄░▄█░░░█▄▄▐▀█▀▌▐▀█░▀█░▀█▄░▐▀░ 
░█▐█▀▐░░▐▄██░░▌░░▐▄▌░░▌░░░██▀░░ 
░█░▌░▐░░▌▀██░░▌░░▐▀▄░░▌░▐▀░▀█▄░ 
▄▌░░░▀░▀░░▀▀░▀▀▀░▀░▀▀░▀░▀░░░░▀▀
░░░░░░░░░░▄▄▄▄▄▄░░░░░░░░░░░░░░░
░░░░░░░░████▀▀███░░░░░░░░░░░░░░
░░░░░░░███░░░░░▀██░░░░░░░░░░░░░
░░░░░░░███▄░▄▄▄▄██░░░░░░░░░░░░░
░░░░░░░████▀▀████▀░░░░░░░░░░░░░
░░░░░░░██▄█▄▄░░░░░░░░░░░░░░░░░░
░░░░░░░░████▄▄░░░░░░░░░░░░░░░░░
░░░░░░░░░██▀░░▄█▄░░░░░░░░░░░░░░
░░░░░░░░░████████▄░░░░░░░░░░░░░
░░░░░░░░░█████████▄▄▄▄░░░░░░░░░
░░░░░░░░▄████████████████▄▄░░░░
░░░░░░▄████████████████████░░░░
░░░▄████████████████████████░░░
░░██████████████████████████▄░░
░████████████████████████████░░
░████████████████████████████░░
░█████████████████████████████░
░█████████████████████████████░
██████████████████████████████░
        What is the Matrix?
-}