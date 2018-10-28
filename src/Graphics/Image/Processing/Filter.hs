{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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
  , lmapFilter
  , rmapFilter
  , dimapFilter
  , applyFilter
  , laplacianFilter
  , laplacianFilterElevated

  , eSobelX
  , genericSobelX

  , laplacian
  , laplacian'
  -- * Sobel
  , sobelHorizontal
  , sobelHorizontalN
  , sobelVertical
  , sobelVerticalN
  , sobelOperator
  , sobelOperatorN
  -- * Prewitt
  , prewittHorizontal
  , prewittVertical
  , prewittOperator
  , prewittOperator'
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

import           Control.Applicative
import           Control.DeepSeq
import           Data.Int
import qualified Data.Massiv.Array       as A
import           Graphics.ColorSpace
import           Graphics.Image.Internal
import           Prelude                 as P

-- | Filter that can be applied to an image using `applyFilter`.
newtype Filter cs a b = Filter
  { filterStencil :: A.Stencil Ix2 (Pixel cs a) (Pixel cs b)
  } deriving (Floating, Fractional, Num, NFData)

type Filter' cs e = Filter cs e e

instance Functor (Pixel cs) => Functor (Filter cs a) where
  fmap f (Filter s) = Filter (fmap (fmap f) s)
  {-# INLINE fmap #-}

instance (ColorSpace cs a, Applicative (Pixel cs)) => Applicative (Filter cs a) where
  pure a = Filter $ pure (pure a)
  {-# INLINE pure #-}
  liftA2 f (Filter x) (Filter y) = Filter (liftA2 (liftA2 f) x y)
  {-# INLINE liftA2 #-}

applyFilter ::
     (ColorSpace cs a, ColorSpace cs b)
  => Border (Pixel cs a)
  -> Filter cs a b
  -> Image cs a
  -> Image cs b
applyFilter border f (Image arr) =
  Image (A.compute (A.mapStencil border (filterStencil f) arr))
{-# INLINE applyFilter #-}

lmapFilter :: (Pixel cs a -> Pixel cs b) -> Filter cs b e -> Filter cs a e
lmapFilter f (Filter s) = Filter (A.lmapStencil f s)
{-# INLINE lmapFilter #-}

rmapFilter :: (Pixel cs a -> Pixel cs b) -> Filter cs e a -> Filter cs e b
rmapFilter g (Filter s) = Filter (A.rmapStencil g s)
{-# INLINE rmapFilter #-}

dimapFilter ::
     (Pixel cs a -> Pixel cs a')
  -> (Pixel cs b -> Pixel cs b')
  -> Filter cs a' b
  -> Filter cs a  b'
dimapFilter f g (Filter s) = Filter (A.dimapStencil f g s)
{-# INLINE dimapFilter #-}


laplacian :: (ColorSpace cs (LevelUp e), ColorSpace cs e) => Filter cs e e
laplacian = rmapFilter (fmap eDown) laplacian'
{-# INLINE laplacian #-}

laplacian' :: (ColorSpace cs (LevelUp e), ColorSpace cs e) => Filter cs e (LevelUp e)
laplacian' =
  Filter $ A.lmapStencil (fmap eUp) $ A.makeStencil (3 :. 3) (1 :. 1) stencil
  where stencil f = - f (-1 :. -1) -     f (-1 :.  0) - f (-1 :.  1) -
                      f ( 0 :. -1) + 8 * f ( 0 :.  0) - f ( 0 :.  1) -
                      f ( 1 :. -1) -     f ( 1 :.  0) - f ( 1 :.  1)
        {-# INLINE stencil #-}
{-# INLINE laplacian' #-}



sobelHorizontal ::
     (ColorSpace cs (LevelUp e), ColorSpace cs e) => Filter cs e (LevelUp e)
sobelHorizontal =
  Filter $ A.lmapStencil (fmap eUp) $ A.makeStencil (3 :. 3) (1 :. 1) $ \ f ->
                f (-1 :. -1) -     f (-1 :.  1) +
            2 * f ( 0 :. -1) - 2 * f ( 0 :.  1) +
                f ( 1 :. -1) -     f ( 1 :.  1)
{-# INLINE sobelHorizontal #-}


sobelHorizontalN ::
     (ColorSpace cs (LevelUp e), ColorSpace cs e) => Filter cs e e
sobelHorizontalN = normalizeSobel sobelHorizontal
{-# INLINE sobelHorizontalN #-}

normalizeSobel ::
     forall cs e. (Elevator (LevelUp e), ColorSpace cs e)
  => Filter cs e (LevelUp e)
  -> Filter cs e e
normalizeSobel = rmapFilter (fmap (\ x -> eDown ((x + 4 * eUp (eMaxValue :: e)) // 8)))
{-# INLINE normalizeSobel #-}

sobelVertical ::
     (ColorSpace cs (LevelUp e), ColorSpace cs e) => Filter cs e (LevelUp e)
sobelVertical =
  Filter $ A.lmapStencil (fmap eUp) $ A.makeStencil (3 :. 3) (1 :. 1) $ \ f ->
          f (-1 :. -1) + 2 * f (-1 :. 0) + f (-1 :. 1)
        - f ( 1 :. -1) - 2 * f ( 1 :. 0) - f ( 1 :. 1)
{-# INLINE sobelVertical #-}


sobelVerticalN ::
     (ColorSpace cs (LevelUp e), ColorSpace cs e) => Filter cs e e
sobelVerticalN = normalizeSobel sobelVertical
{-# INLINE sobelVerticalN #-}


-- sobelOperator ::
--      (ColorSpace cs (LevelUp e), ColorSpace cs e) => Filter cs e Double
-- sobelOperator = fmap sqrt $ liftA2 (+) (fmap sqrd sobelHorizontal) (fmap sqrd sobelVertical)
--   where
--     sqrd x =
--       let xd = eCoerceToDouble x
--        in xd * xd
--     {-# INLINE sqrd #-}
-- {-# INLINE sobelOperator #-}


sobelOperator :: (ColorSpace cs Double) => Filter' cs Double
sobelOperator =
  fmap sqrt $ liftA2 (+) (fmap sqr sobelHorizontal) (fmap sqr sobelVertical)
  where
    sqr x = x * x
    {-# INLINE sqr #-}
{-# INLINE sobelOperator #-}

sobelOperatorN :: (ColorSpace cs Double) => Filter' cs Double
sobelOperatorN = fmap (/ sqrt 32) sobelOperator
{-# INLINE sobelOperatorN #-}


prewittHorizontal ::
     (ColorSpace cs (LevelUp e), ColorSpace cs e) => Filter cs e (LevelUp e)
prewittHorizontal =
  Filter $ A.lmapStencil (fmap eUp) $ A.makeStencil (3 :. 3) (1 :. 1) $ \ f ->
                f (-1 :. -1) - f (-1 :.  1) +
                f ( 0 :. -1) - f ( 0 :.  1) +
                f ( 1 :. -1) - f ( 1 :.  1)
{-# INLINE prewittHorizontal #-}


prewittVertical ::
     (ColorSpace cs (LevelUp e), ColorSpace cs e) => Filter cs e (LevelUp e)
prewittVertical =
  Filter $ A.lmapStencil (fmap eUp) $ A.makeStencil (3 :. 3) (1 :. 1) $ \ f ->
          f (-1 :. -1) + f ( 0 :. -1) + f ( 1 :. -1)
        - f (-1 :.  1) - f ( 0 :.  1) - f ( 1 :.  1)
{-# INLINE prewittVertical #-}


prewittOperator ::
     (ColorSpace cs (LevelUp e), ColorSpace cs e) => Filter cs e Double
prewittOperator =
  fmap (sqrt . eCoerceToDouble) $
  liftA2 (+) (fmap (^ (2 :: Int)) prewittHorizontal) (fmap (^ (2 :: Int)) prewittVertical)
{-# INLINE prewittOperator #-}


prewittOperator' ::
     (ColorSpace cs (LevelUp e), ColorSpace cs e) => Filter cs e Double
prewittOperator' = rmapFilter (fmap (/ sqrt 18)) prewittOperator
{-# INLINE prewittOperator' #-}


----------------------------------
------- Benchmarking -------------
----------------------------------


-- laplacian :: (ColorSpace cs (LevelUp e), ColorSpace cs e) => Filter cs e e
-- laplacian =
--   Filter $
--   A.dimapStencil
--     (fmap eUp) (fmap eDown) $
--   (A.makeStencil (3 :. 3) (1 :. 1) $ \f' ->
--                     let f ix = f' ix in -- <- Very strange, doubles the performance
--                     (-1) * f (-1 :. -1) -       f (-1 :.  0) - f (-1 :.  1) -
--                            f ( 0 :. -1) + 8  *  f ( 0 :.  0) - f ( 0 :.  1) -
--                            f ( 1 :. -1) -       f ( 1 :.  0) - f ( 1 :.  1))
-- {-# INLINE laplacian #-}


type CommonColorSpace cs
   = ( ColorSpace cs Int16
     , ColorSpace cs Int32
     , ColorSpace cs Word32
     , ColorSpace cs Word64
     , ColorSpace cs Float
     , ColorSpace cs Double)

laplacianFilter :: (CommonColorSpace cs, ColorSpace cs e) => Filter' cs e
laplacianFilter = laplacianFilterGeneric eCoerceToDouble eRoundFromDouble
{-# INLINE [2] laplacianFilter #-}

{-# RULES
"laplacianFilter/Word8" [~2] laplacianFilter = laplacianFilter8
"laplacianFilter/Word16" [~2] laplacianFilter = laplacianFilter16
 #-}

-- TODO: benchmark
laplacianFilter8 :: (ColorSpace cs Int16, ColorSpace cs Word8) => Filter' cs Word8
laplacianFilter8 =
  laplacianFilterGeneric
    (fromIntegral :: Word8 -> Int16)
    (fromIntegral :: Int16 -> Word8)
{-# INLINE [2] laplacianFilter8 #-}

-- TODO: benchmark
laplacianFilter16 :: (ColorSpace cs Int32, ColorSpace cs Word16) => Filter' cs Word16
laplacianFilter16 =
  laplacianFilterGeneric (fromIntegral :: Word16 -> Int32) (fromIntegral :: Int32 -> Word16)
{-# INLINE [2] laplacianFilter16 #-}


laplacianFilterGeneric ::
     (ColorSpace cs a, ColorSpace cs b) => (a -> b) -> (b -> a) -> Filter' cs a
laplacianFilterGeneric to from =
  Filter $
  A.makeStencil (3 :. 3) (1 :. 1) $ \f' ->
    let f ix = fmap to <$> f' ix
     in fmap from <$> ((-1) * f (-1 :. -1) -     f (-1 :.  0) - f (-1 :.  1) -
                              f ( 0 :. -1) + 8 * f ( 0 :.  0) - f ( 0 :.  1) -
                              f ( 1 :. -1) -     f ( 1 :.  0) - f ( 1 :.  1))
{-# INLINE laplacianFilterGeneric #-}

laplacianFilterElevated ::
     (ColorSpace cs (LevelUp a), ColorSpace cs a) => Filter' cs a
laplacianFilterElevated = -- laplacianFilterGeneric eUp eDown
  Filter
    (A.makeStencil (3 :. 3) (1 :. 1) $ \f' ->
       let f ix = fmap eUp <$> f' ix
        in fmap eDown <$>
           (8 * f (0 :. 0) +
            (-1) *
            (f (-1 :. -1) + f (-1 :. 0) + f (-1 :. 1) + f (0 :. -1) + f (0 :. 1) + f (1 :. -1) +
             f (1 :. 0) +
             f (1 :. 1))))
{-# INLINE laplacianFilterElevated #-}




genericSobelX ::
     (ColorSpace cs b, ColorSpace cs a) => (a -> b) -> (b -> a) -> Filter' cs a
genericSobelX to from =
  Filter $ A.makeStencil (3 :. 3) (1 :. 1) $ \ f ->
    fmap from <$> (
          (fmap to <$> f (-1 :. -1))        +
          (fmap to <$> f ( 0 :. -1)) *   2  +
          (fmap to <$> f ( 1 :. -1))        +
          (fmap to <$> f (-1 :.  1)) * (-1) +
          (fmap to <$> f ( 0 :.  1)) * (-2) +
          (fmap to <$> f ( 1 :.  1)) * (-1))
{-# INLINE genericSobelX #-}


eSobelX ::
     (ColorSpace cs (LevelUp a), ColorSpace cs a) => Filter' cs a
eSobelX =
  Filter $ A.makeStencil (3 :. 3) (1 :. 1) $ \ f ->
    fmap eDown <$> (
          (fmap eUp <$> f (-1 :. -1))        +
          (fmap eUp <$> f ( 0 :. -1)) *   2  +
          (fmap eUp <$> f ( 1 :. -1))        +
          (fmap eUp <$> f (-1 :.  1)) * (-1) +
          (fmap eUp <$> f ( 0 :.  1)) * (-2) +
          (fmap eUp <$> f ( 1 :.  1)) * (-1))
{-# INLINE eSobelX #-}
