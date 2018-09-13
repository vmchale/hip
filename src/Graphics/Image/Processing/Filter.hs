{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
-- |
-- Module      : Graphics.Image.Processing.Filter
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Filter
  ( -- * Filter
    Filter
  , applyFilter
  -- , Direction(..)
  --   -- * Gaussian
  -- , gaussianLowPass
  -- , gaussianBlur
  --   -- * Sobel
  -- , sobelFilter
  -- , sobelOperator
  --   -- * Prewitt
  -- , prewittFilter
  -- , prewittOperator
  ) where

import           Control.DeepSeq
import qualified Data.Massiv.Array       as A
import           Graphics.ColorSpace
import           Graphics.Image.Internal
import           Prelude                 as P
import Data.Int


-- | Filter that can be applied to an image using `applyFilter`.
--
-- @since 1.5.3
newtype Filter cs e = Filter
  { filterStencil :: A.Stencil Ix2 (Pixel cs e) (Pixel cs e)
  } deriving (Floating, Fractional, Num, NFData)


applyFilter :: ColorSpace cs e => Border (Pixel cs e) -> Filter cs e -> Image cs e -> Image cs e
applyFilter border (Filter stencil) (Image arr) =
  Image (A.compute (A.mapStencil border stencil arr))


-- IDEA: rerwrite rules depending on types

type CommonColorSpace cs
   = ( ColorSpace cs Word8
     , ColorSpace cs Int16
     , ColorSpace cs Word32
     , ColorSpace cs Word64
     , ColorSpace cs Float
     , ColorSpace cs Double)

laplacianFilter :: (CommonColorSpace cs, ColorSpace cs e) => Filter cs e
laplacianFilter =
  Filter (A.makeStencil (3 :. 3) (1 :. 1) $ \ f' ->
             let f ix = toDouble <$> f' ix
             in fromDouble <$> (
               8  *   f ( 0 :.  0) +
             (-1) * ( f (-1 :. -1) + f (-1 :. 0) + f (-1 :. 1)
                    + f ( 0 :. -1) +               f ( 0 :. 1)
                    + f ( 1 :. -1) + f ( 1 :. 0) + f ( 1 :. 1))))
{-# INLINE [2] laplacianFilter #-}

{-# RULES
"laplacianFilter/Word8" [~2] laplacianFilter = laplacianFilter8
 #-}

-- TODO: benchmark
laplacianFilter8 :: (ColorSpace cs Int16, ColorSpace cs Word8) => Filter cs Word8
laplacianFilter8 =
  Filter
    (A.makeStencil (3 :. 3) (1 :. 1) $ \f' ->
       let f ix = fmap (fromIntegral :: Word8 -> Int16) <$> f' ix
        in fmap (fromIntegral :: Int16 -> Word8) <$>
           (8 * f (0 :. 0) +
            (-1) *
            (f (-1 :. -1) + f (-1 :. 0) + f (-1 :. 1) + f (0 :. -1) + f (0 :. 1) + f (1 :. -1) +
             f (1 :. 0) +
             f (1 :. 1))))
{-# INLINE [2] laplacianFilter8 #-}


-- -- | Used to specify direction for some filters.
-- data Direction
--   = Vertical
--   | Horizontal

-- -- | Create a Gaussian Filter.
-- --
-- -- @since 1.5.3
-- gaussianLowPass :: (Array arr cs e, Array arr X e, Floating e, Fractional e) =>
--                    Int -- ^ Radius
--                 ->+ e -- ^ Sigma
--                 -> Border (Pixel cs e) -- ^ Border resolution technique.
--                 -> Filter arr cs e
-- gaussianLowPass !r !sigma border =
--   Filter (correlate border gV' . correlate border gV)
--   where
--     !gV = compute $ (gauss / scalar weight)
--     !gV' = compute $ transpose gV
--     !gauss = makeImage (1, n) getPx
--     !weight = I.fold (+) 0 gauss
--     !n = 2 * r + 1
--     !sigma2sq = 2 * sigma ^ (2 :: Int)
--     getPx (_, j) = promote $ exp (fromIntegral (-((j - r) ^ (2 :: Int))) / sigma2sq)
--     {-# INLINE getPx #-}
-- {-# INLINE gaussianLowPass #-}



-- -- | Create a Gaussian Blur filter. Radius will be derived from standard
-- -- deviation: @ceiling (2*sigma)@ and `Edge` border resolution will be
-- -- utilized. If custom radius and/or border resolution is desired,
-- -- `gaussianLowPass` can be used instead.
-- --
-- -- @since 1.5.3
-- gaussianBlur :: (Array arr cs e, Array arr X e, Floating e, RealFrac e) =>
--                 e -- ^ Sigma
--              -> Filter arr cs e
-- gaussianBlur !sigma = gaussianLowPass (ceiling (2*sigma)) sigma Edge
-- {-# INLINE gaussianBlur #-}


-- sobelFilter :: (Array arr cs e, Array arr X e) =>
--                Direction -> Border (Pixel cs e) -> Filter arr cs e
-- sobelFilter dir !border =
--   Filter (correlate border kernel)
--   where
--     !kernel =
--       case dir of
--         Vertical   -> fromLists $ [ [ -1, -2, -1 ]
--                                   , [  0,  0,  0 ]
--                                   , [  1,  2,  1 ] ]
--         Horizontal -> fromLists $ [ [ -1, 0, 1 ]
--                                   , [ -2, 0, 1 ]
--                                   , [ -1, 0, 1 ] ]
-- {-# INLINE sobelFilter #-}

-- -- sobelFilter :: Array arr cs e =>
-- --                Direction -> Border (Pixel cs e) -> Filter arr cs e
-- -- sobelFilter dir !border =
-- --   Filter (convolveCols border cV . convolveRows border rV)
-- --   where
-- --     !(rV, cV) =
-- --       case dir of
-- --         Vertical   -> ([1, 2, 1], [1, 0, -1])
-- --         Horizontal -> ([1, 0, -1], [1, 2, 1])
-- -- {-# INLINE sobelFilter #-}


-- sobelOperator :: (Array arr cs e, Array arr X e, Floating e) => Image arr cs e -> Image arr cs e
-- sobelOperator !img = sqrt (sobelX ^ (2 :: Int) + sobelY ^ (2 :: Int))
--   where !sobelX = applyFilter (sobelFilter Horizontal Edge) img
--         !sobelY = applyFilter (sobelFilter Vertical Edge) img
-- {-# INLINE sobelOperator #-}




-- prewittFilter :: (Array arr cs e, Array arr X e) =>
--                  Direction -> Border (Pixel cs e) -> Filter arr cs e
-- prewittFilter dir !border =
--   Filter (convolveCols border cV . convolveRows border rV)
--   where
--     !(rV, cV) =
--       case dir of
--         Vertical   -> ([1, 1, 1], [1, 0, -1])
--         Horizontal -> ([1, 0, -1], [1, 1, 1])
-- {-# INLINE prewittFilter #-}



-- prewittOperator :: (Array arr cs e, Array arr X e, Floating e) => Image arr cs e -> Image arr cs e
-- prewittOperator !img = sqrt (prewittX ^ (2 :: Int) + prewittY ^ (2 :: Int))
--   where !prewittX = applyFilter (prewittFilter Horizontal Edge) img
--         !prewittY = applyFilter (prewittFilter Vertical Edge) img
-- {-# INLINE prewittOperator #-}

