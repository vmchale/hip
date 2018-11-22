{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Graphics.Image.IO
-- Copyright   : (c) Alexey Kuleshevich 2016-2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.IO
  (  -- * Reading
    readImage
  , readImageAuto
  , readImageY
  , readImageYA
  , readImageRGB
  , readImageRGBA
  -- * Writing
  , writeImage
  , writeImageAuto
  -- TODO: reexport bunch of massiv-io stuff
  -- * Displaying
  , A.ExternalViewer(..)
  , displayImage
  , displayImageUsing
  -- ** Common viewers
  , A.displayImageFile
  , A.defaultViewer
  , A.eogViewer
  , A.gpicviewViewer
  , A.fehViewer
  , A.gimpViewer
  -- * Supported Image Formats
      -- ** BMP
  , A.BMP(..)
    -- ** GIF
  , A.GIF(..)
  , A.WriteOptionsGIF
  , A.woGetPaletteOptionsGIF
  , A.woSetPaletteOptionsGIF
  , A.PaletteOptions(..)
  , A.PaletteCreationMethod(..)
    -- *** Animated
  , A.WriteOptionsSequenceGIF
  , A.woGetGifLoopingGIFs
  , A.woGetPaletteOptionsGIFs
  , A.woSetGifLoopingGIFs
  , A.woSetPaletteOptionsGIFs
  , A.GifDelay
  , A.GifLooping(..)
  -- ** HDR
  , A.HDR(..)
  -- ** JPG
  , A.JPG(..)
  , A.WriteOptionsJPG
  , A.woGetQualityJPG
  , A.woSetQualityJPG
  -- ** PNG
  , A.PNG(..)
  -- ** TGA
  , A.TGA(..)
  -- ** TIF
  , A.TIF(..)
  -- ** PBM
  , A.PBM(..)
  -- ** PGM
  , A.PGM(..)
  -- ** PPM
  , A.PPM(..)
  -- module Graphics.Image.IO.Formats
  -- $supported
  -- * Hands on examples
  -- ** Animated GIF
  -- $animation
  ) where

import           Control.Monad.IO.Class  (MonadIO (..))
import qualified Data.Massiv.Array       as A
import qualified Data.Massiv.Array.IO    as A
import           Graphics.ColorSpace
import           Graphics.Image.Internal

-- | Display an image by writing it as a .tiff file to a temporary directory and making a call to an
-- external viewer that is set as a default image viewer by the OS.
displayImage :: (MonadIO m, ToRGBA cs e) => Image cs e -> m ()
displayImage (Image arr) = liftIO $ A.displayImage arr
{-# NOINLINE displayImage #-}

-- | Display an image by making a call to an external viewer that is set as a default image viewer
-- by the OS.
displayImageUsing :: (MonadIO m, ToRGBA cs e) => A.ExternalViewer -> Bool -> Image cs e -> m ()
displayImageUsing ev block (Image arr) = liftIO $ A.displayImageUsing ev block arr
{-# NOINLINE displayImageUsing #-}


-- | Try to guess an image format from file's extension, then attempt to decode it as such. Color
-- space and precision of the result array must match exactly that of the actual image, in order to
-- apply auto conversion use `readImageAuto` instead.
--
-- Might throw `A.ConvertError`, `A.DecodeError`, besides other standard errors related to file IO.
--
-- Resulting image will be read as specified by the type signature:
--
-- >>> frog <- readImage "images/frog.jpg" :: IO (Image YCbCr Word8)
-- >>> displayImage frog
readImage ::
     (MonadIO m, ColorSpace cs e)
  => FilePath -- ^ File path for an image
  -> m (Image cs e)
readImage path = fmap (Image . A.setComp Par) (liftIO (A.readImage path))
{-# NOINLINE readImage #-}


-- | Same as `readImage`, but will perform any possible color space and
-- precision conversions in order to match the result image type. Very useful
-- whenever image format isn't known at compile time.
readImageAuto :: (MonadIO m, ColorSpace cs e) =>
                  FilePath -- ^ File path for an image
               -> m (Image cs e)
readImageAuto path = fmap (Image . A.setComp Par) (liftIO (A.readImageAuto path))
{-# INLINE readImageAuto #-}


-- | Read image as luma (brightness), i.e. grayscale.
readImageY :: MonadIO m => FilePath -> m (Image Y Double)
readImageY = readImageAuto
{-# INLINE readImageY #-}


-- | Read image as luma with 'Alpha' channel.
readImageYA :: MonadIO m => FilePath -> m (Image YA Double)
readImageYA = readImageAuto
{-# INLINE readImageYA #-}


-- | Read image in RGB colorspace.
readImageRGB :: MonadIO m => FilePath -> m (Image RGB Double)
readImageRGB = readImageAuto
{-# INLINE readImageRGB #-}


-- | Read image in RGB colorspace with 'Alpha' channel.
readImageRGBA :: MonadIO m => FilePath -> m (Image RGBA Double)
readImageRGBA = readImageAuto
{-# INLINE readImageRGBA #-}




-- | Inverse of the 'readImage', but similarly to it, will guess an output file format from the file
-- extension and will write to file any image with the colorspace that is supported by that
-- format. Precision of the image might be adjusted using `Elevator` whenever precision of the
-- source image is not supported by the image file format. For instance, @`Image` `RGBA` `Double`@
-- being saved as 'PNG' file would be written as @`Image` `RGBA` `Word16`@, thus using highest
-- supported precision `Word16` for that format. If automatic colors space conversion is also
-- desired, `writeImageAuto` can be used instead.
--
-- Can throw `A.ConvertError`, `A.EncodeError` and other usual IO errors.
--
writeImage :: (MonadIO m, ColorSpace cs e) =>
               FilePath -> Image cs e -> m ()
writeImage path (Image arr) = liftIO (A.writeImage path arr)
{-# NOINLINE writeImage #-}


-- | Write any image while doing any necessary color space and precision conversions.
writeImageAuto ::
     (MonadIO m, ToYA cs e, ToRGBA cs e, ToYCbCr cs e, ToCMYK cs e)
  => FilePath
  -> Image cs e
  -> m ()
writeImageAuto path (Image arr) = liftIO (A.writeImageAuto path arr)
{-# NOINLINE writeImageAuto #-}

