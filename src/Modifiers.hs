module Modifiers 
    ( modifiersList,
      negative,
      grayscale,
      gamma,
      noRed,
      noGreen,
      noBlue,
      onlyRed,
      onlyGreen,
      onlyBlue
    ) where 

import Codec.Picture

modifiersList = "Modifiers List (to apply add the argument --do:MODIFIER-CODE)\n" ++
               "Filters:\n" ++
               " neg - Negative (Inversion)\n" ++ 
               " grayscale - Grayscale\n" ++ 
               " gamma-N - Gamma Correction (N is your coefficient)\n" ++
               " aquarel - Aquarelisation\n" ++ 
               " emboss - Embossing\n" ++
               " sharp - Increase Sharpness\n" ++
               " blur - Gaussian Blur\n" ++ 
               " no-red / no-green / no-blue - Disable Red/Green/Blue Channel\n" ++
               " only-red / only-green / only-blue - Only Red/Green/Blue Channel\n" ++
               "Transformation & Rotation:\n" ++
               " rotate-N - N Degrees Rotation\n" ++
               " crop - 2X Zoom\n"

rounding :: (RealFrac a, Integral b) => a -> b
rounding a = floor (a + 0.5)

pxify = rounding

pxMult :: Pixel8 -> Double -> Double
pxMult p x = fromIntegral p * x

pxPow :: Pixel8 -> Double -> Double 
pxPow p x = fromIntegral p ** x

pxDiv :: Pixel8 -> Double -> Double 
pxDiv p x = fromIntegral p / x

negative :: Image PixelRGBA8 -> Image PixelRGBA8
negative = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 (255 - r) (255 - g) (255 - b) a

grayscale :: Image PixelRGBA8 -> Image PixelRGBA8
grayscale = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 (y r g b) (y r g b) (y r g b) a 
  where y r g b = pxify $ pxMult r 0.2126 + pxMult g 0.7152 + pxMult b 0.0722

gamma :: Double  -> Image PixelRGBA8 -> Image PixelRGBA8
gamma n = pixelMap $ \(PixelRGBA8 r g b a) -> PixelRGBA8 (y r) (y g) (y b) a
  where y p = pxify $ (pxDiv p 255 ** (1 / n)) * 255
  





-- RGB Channels Manipulations
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
-- end