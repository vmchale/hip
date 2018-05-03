{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Graphics.Image.Processing.Binary
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Binary
  ( -- * Thresholding
    threshold
  , threshold2
  , thresholdWith
  , thresholdWith2
  , Thresholding
  -- ** Common comparators
  , (!==!)
  , (!/=!)
  , (!<!)
  , (!<=!)
  , (!>!)
  , (!>=!)
  , (!&&!)
  , (!||!)
  , (.==.)
  , (./=.)
  , (.<.)
  , (.<=.)
  , (.>.)
  , (.>=.)
  , (.&&.)
  , (.||.)
  -- * Bitwise operations
  , or
  , and
  , invert
  , disjunction
  , conjunction
  -- * Binary Morphology
  -- $morphology
  , erode
  , dialate
  , open
  , close
  ) where

import           Control.Applicative
import           Data.Bits
import qualified Data.Foldable                         as F
import           Graphics.ColorSpace
import           Graphics.Image.Internal               as I
import           Graphics.Image.Processing.Convolution
import           Prelude                               as P hiding (and, or)

infix  4  .==., ./=., .<., .<=., .>=., .>., !==!, !/=!, !<!, !<=!, !>=!, !>!
-- infixr 3  .&&., !&&!
-- infixr 2  .||., !||!



-- | 'Thresholding' contains a convenient set of functions for binary image
-- construction, which is done by comparing either a single pixel with every
-- pixel in an image or two same size images pointwise. For example:
--
-- >>> frog <- readImageY "images/frog.jpg"
-- >>> frog .==. PixelY 0    -- (or: PixelY 0 .==. frog)
-- >>> frog .<. flipH frog   -- (or: flipH frog .>. frog)
--
class Thresholding a b where
  threshold2 ::
       (ColorSpace cs e', ColorSpace cs e, ColorSpace cs Bit)
    => Pixel cs (e' -> e -> Bool)
    -> a cs e'
    -> b cs e
    -> Image cs Bit
  thresholdWith2 ::
       (ColorSpace cs e)
    => (Pixel cs e -> Pixel cs e -> Bool) -- ^ Predicate
    -> a cs e -- ^ First source image.
    -> b cs e -- ^ Second source image.
    -> Image X Bit



instance Thresholding Image Image where
  threshold2 f = I.zipWith (\ px1 px2 -> bool2bit <$> (f <*> px1 <*> px2))
  {-# INLINE threshold2 #-}
  thresholdWith2 f = I.zipWith (\ px1 px2 -> fromBool (f px1 px2))
  {-# INLINE thresholdWith2 #-}


instance Thresholding Pixel Image where
  threshold2 f px1 = I.map (\ px2 -> bool2bit <$> (f <*> px1 <*> px2))
  {-# INLINE threshold2 #-}
  thresholdWith2 f px1 = I.map (\ px2 -> fromBool (f px1 px2))
  {-# INLINE thresholdWith2 #-}


instance Thresholding Image Pixel where
  threshold2 f img px2 = I.map (\ px1 -> bool2bit <$> (f <*> px1 <*> px2)) img
  {-# INLINE threshold2 #-}
  thresholdWith2 f img px2 = I.map (\ px1 -> fromBool (f px1 px2)) img
  {-# INLINE thresholdWith2 #-}


(!==!), (!/=!) ::
     (Thresholding a b, ColorSpace cs e, ColorSpace cs Bit) => a cs e -> b cs e -> Image cs Bit
(!==!) = threshold2 (pure (==))
{-# INLINE (!==!) #-}
(!/=!) = threshold2 (pure (/=))
{-# INLINE (!/=!) #-}

(!<!), (!<=!), (!>!), (!>=!) ::
     (Thresholding a b, ColorSpace cs e, ColorSpace cs Bit, Ord e)
  => a cs e
  -> b cs e
  -> Image cs Bit
(!<!)  = threshold2 (pure (<))
{-# INLINE (!<!) #-}
(!<=!) = threshold2 (pure (<=))
{-# INLINE (!<=!) #-}
(!>!)  = threshold2 (pure (>))
{-# INLINE (!>!) #-}
(!>=!) = threshold2 (pure (>=))
{-# INLINE (!>=!) #-}


(.==.), (./=.) :: (Thresholding a b, ColorSpace cs e) => a cs e -> b cs e -> Image X Bit
(.==.) = thresholdWith2 (==)
{-# INLINE (.==.) #-}
(./=.) = thresholdWith2 (/=)
{-# INLINE (./=.) #-}

(.<.), (.<=.), (.>.), (.>=.) ::
     (Thresholding a b, ColorSpace cs e, Ord (Pixel cs e)) => a cs e -> b cs e -> Image X Bit
(.<.)  = thresholdWith2 (<)
{-# INLINE (.<.) #-}
(.<=.) = thresholdWith2 (<=)
{-# INLINE (.<=.) #-}
(.>.)  = thresholdWith2 (>)
{-# INLINE (.>.) #-}
(.>=.) = thresholdWith2 (>=)
{-# INLINE (.>=.) #-}


-- TODO: validate optimization rewrite rule:
--{-# RULES
--"bit/bool/binary" forall x. fromBool (bit2bool x) = pure x
-- #-}
-- | Pixel wise @AND@ operator on binary images. Unlike `!&&!` this operator
-- will also @AND@ pixel componenets.
(.&&.), (.||.) :: (Thresholding a b, ColorSpace cs Bit) =>
                  a cs Bit -> b cs Bit -> Image X Bit
(.&&.) = thresholdWith2 (\px1 px2 -> bit2bool $ foldlPx2 (const (.&.)) one px1 px2)
{-# INLINE (.&&.) #-}

-- | Pixel wise @OR@ operator on binary images. Unlike `!||!` this operator
-- will also @OR@ pixel componenets.
(.||.) = thresholdWith2 (\px1 px2 -> bit2bool $ foldlPx2 (const (.|.)) one px1 px2)
{-# INLINE (.||.) #-}


-- TODO: validate optimization rewrite rule:
--{-# RULES
--"bit/bool/bit" forall x. bool2bit (bit2bool x) = x
-- #-}
-- | Pixel wise @AND@ operator on binary images.
(!&&!), (!||!) :: (Thresholding a b, ColorSpace cs Bit) =>
          a cs Bit -> b cs Bit -> Image cs Bit
(!&&!) = threshold2 (pure (\e1 e2 -> bit2bool (e1 .&. e2)))
{-# INLINE (!&&!) #-}

-- | Pixel wise @OR@ operator on binary images.
(!||!) = threshold2 (pure (\e1 e2 -> bit2bool (e1 .|. e2)))
{-# INLINE (!||!) #-}


-- | Complement each pixel in a binary image
invert :: ColorSpace cs Bit => Image cs Bit -> Image cs Bit
invert = I.map (fmap complement)
{-# INLINE invert #-}


-- | Construct a binary image using a predicate from a source image.
thresholdWith ::
     ColorSpace cs e
  => (Pixel cs e -> Bool) -- ^ Predicate
  -> Image cs e -- ^ Source image.
  -> Image X Bit
thresholdWith f = I.map (fromBool . f)
{-# INLINE thresholdWith #-}


-- | Threshold an image by supplying a thresholding function per channel.
threshold :: (ColorSpace cs e, ColorSpace cs Bit) =>
             Pixel cs (e -> Bool) -> Image cs e -> Image cs Bit
threshold f = I.map (fmap bool2bit . (f <*>))
{-# INLINE threshold #-}


-- | Join each component of a pixel with a binary @`.|.`@ operator.
disjunction, conjunction :: (ColorSpace cs Bit) => Image cs Bit -> Image X Bit
disjunction = I.map (PixelX . F.foldl' (.|.) zero)
{-# INLINE disjunction #-}

-- | Join each component of a pixel with a binary @`.&.`@ operator.
conjunction = I.map (PixelX . F.foldl' (.&.) one)
{-# INLINE conjunction #-}

-- | Disjunction of all pixels in a Binary image
or :: Image X Bit -> Bool
or = isOn . fold (.|.) off
{-# INLINE or #-}

-- | Conjunction of all pixels in a Binary image
and :: Image X Bit -> Bool
and = isOn . fold (.&.) on
{-# INLINE and #-}


{- $morphology In order to demonstrate how morphological operations work, a
/binary source image/ = __B__ constructed here together with a /structuring element/
= __S__ will be used in examples that follow. Origin of the structuring
element is always at it's center, eg. @(1,1)@ for the one below.

@
figure :: Image VU X Bit
figure = fromLists [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,1,0],
                    [0,0,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0],
                    [0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0],
                    [0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0],
                    [0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0],
                    [0,0,0,0,0,0,1,1,1,1,0,0,0,1,0,0,0],
                    [0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0],
                    [0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]
struct :: Image VU X Bit
struct = fromLists [[0,1,0],[1,1,0],[0,1,0]]
@
-}


-- | Erosion is defined as: __{E = B ⊖ S = {m,n|Sₘₙ⊆B}__
--
-- >>> writeImageExact PNG [] "images/figure_erode.png" $ pixelGrid 10 $ fromImageBinary $ erode struct figure
--
-- <<images/figure.png>> eroded with <<images/struct.png>> is <<images/figure_erode.png>>
--
erode :: ColorSpace cs Bit
      => Image cs Bit -- ^ Structuring element.
      -> Image cs Bit -- ^ Binary source image.
      -> Image cs Bit
erode struc = invert . convolve (Fill (pure one)) struc . invert
{-# INLINE erode #-}


-- | Dialation is defined as: __{D = B ⊕ S = {m,n|Sₘₙ∩B≠∅}__
--
-- >>> writeImageExact PNG [] "images/figure_dialate.png" $ pixelGrid 10 $ fromImageBinary $ dialate struct figure
--
-- <<images/figure.png>> dialated with <<images/struct.png>> is <<images/figure_dialate.png>>
--
dialate :: ColorSpace cs Bit
        => Image cs Bit -- ^ Structuring element.
        -> Image cs Bit -- ^ Binary source image.
        -> Image cs Bit
dialate = convolve (Fill (pure zero))
{-# INLINE dialate #-}


-- | Opening is defined as: __{B ○ S = (B ⊖ S) ⊕ S}__
--
-- >>> writeImageExact PNG [] "images/figure_open.png" $ pixelGrid 10 $ fromImageBinary $ open struct figure
--
-- <<images/figure.png>> opened with <<images/struct.png>> is <<images/figure_open.png>>
--
open :: ColorSpace cs Bit
     => Image cs Bit -- ^ Structuring element.
     -> Image cs Bit -- ^ Binary source image.
     -> Image cs Bit
open struc = dialate struc . erode struc
{-# INLINE open #-}


-- | Closing is defined as: __{B ● S = (B ⊕ S) ⊖ S}__
--
-- >>> writeImageExact PNG [] "images/figure_close.png" $ pixelGrid 10 $ fromImageBinary $ close struct figure
--
-- <<images/figure.png>> closed with <<images/struct.png>> is <<images/figure_close.png>>
--
close :: ColorSpace cs Bit
      => Image cs Bit -- ^ Structuring element.
      -> Image cs Bit -- ^ Binary source image.
      -> Image cs Bit
close struc = erode struc . dialate struc
{-# INLINE close #-}

