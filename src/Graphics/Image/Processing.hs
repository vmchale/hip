{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Graphics.Image.Processing
-- Copyright   : (c) Alexey Kuleshevich 2016-2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing
  (
  -- * Geometric
  module Graphics.Image.Processing.Geometric
  -- * Interpolation
  , module Graphics.Image.Processing.Interpolation
  -- * Convolution
  , module Graphics.Image.Processing.Convolution
  -- * Binary
  , module Graphics.Image.Processing.Binary
  -- * Complex
  , module Graphics.Image.Processing.Complex
  -- * Filters
  , module Graphics.Image.Processing.Filter
  -- * Tools
  , pixelGrid
  ) where

import Graphics.ColorSpace
import Graphics.Image.Internal
import Graphics.Image.Processing.Binary
import Graphics.Image.Processing.Complex
import Graphics.Image.Processing.Convolution
import Graphics.Image.Processing.Geometric
import Graphics.Image.Processing.Interpolation
import Prelude hiding (traverse)
import Graphics.Image.Processing.Filter



-- | This function magnifies an image by a positive factor and draws a grid
-- around the original pixels. It is here simply as useful inspection tool.
--
-- >>> frog <- readImageRGB VU "images/frog.jpg"
-- >>> writeImage "images/frog_eye_grid.png" $ pixelGrid 10 $ crop (51, 112) (20, 20) frog
--
-- <<images/frog.jpg>> <<images/frog_eye_grid.png>>
--
pixelGrid :: ColorSpace cs e =>
             Int          -- ^ Magnification factor.
          -> Image cs e -- ^ Source image.
          -> Image cs e
pixelGrid (succ -> k) = traverse getNewDims getNewPx
  where
    getNewDims (m :. n) = 1 + m * k :. 1 + n * k
    {-# INLINE getNewDims #-}
    getNewPx getPx (i :. j) =
      if i `mod` k == 0 || j `mod` k == 0
        then pure $ eFromDouble 0.5
        else getPx ((i - 1) `div` k :. (j - 1) `div` k)
    {-# INLINE getNewPx #-}
{-# INLINE pixelGrid #-}

