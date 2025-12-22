module AOC.Image (upScale) where

import Codec.Picture (Image, Pixel, generateImage, imageHeight, imageWidth, pixelAt)

-- import Codec.Picture.Types (Image, Pixel)

upScale :: (Pixel a) => Int -> Image a -> Image a
upScale n img = generateImage getPixel' (imageWidth img * n) (imageHeight img * n)
  where
    getPixel' x y = pixelAt img (div x n) (div y n)
