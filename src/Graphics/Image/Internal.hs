{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Module      : Graphics.Image.Internal
-- Copyright   : (c) Alexey Kuleshevich 2017-2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Internal where

import           Control.Monad.IO.Class   (MonadIO (..))
import qualified Data.Massiv.Array        as A
import qualified Data.Massiv.Array.IO     as A
import           Data.Massiv.Array.Unsafe as A
import           Data.Massiv.Core
import           Data.Monoid
import           Data.Typeable
import           Graphics.ColorSpace
import           Prelude                  as P hiding (map)

data Image cs e = Image !(Array A.S Ix2 (Pixel cs e))


instance ColorSpace cs e => Show (Image cs e) where
  show img =
    let (m :. n) = dims img
    in "<Image " ++
       showsTypeRep (typeRep (Proxy :: Proxy cs)) " (" ++
       showsTypeRep (typeRep (Proxy :: Proxy e)) "): " ++ show m ++ "x" ++ show n ++ ">"

instance ColorSpace cs e => Eq (Image cs e) where
  (==) img1 img2 = delayI img1 == delayI img2
  {-# INLINE [~1] (==) #-}


instance ColorSpace cs e => Num (Image cs e) where
  (+)         = liftImage2 (+)
  {-# INLINE [~1] (+) #-}
  (-)         = liftImage2 (-)
  {-# INLINE [~1] (-) #-}
  (*)         = liftImage2 (*)
  {-# INLINE [~1] (*) #-}
  abs         = map abs
  {-# INLINE [~1] abs #-}
  signum      = map signum
  {-# INLINE [~1] signum #-}
  fromInteger = scalar . fromInteger
  {-# INLINE [~1] fromInteger #-}

instance (Fractional e, ColorSpace cs e) => Fractional (Image cs e) where
  (/) = liftImage2 (/)
  {-# INLINE [~1] (/) #-}
  fromRational = scalar . fromRational
  {-# INLINE [~1] fromRational #-}


instance (Floating e, ColorSpace cs e) => Floating (Image cs e) where
  pi = scalar pi
  {-# INLINE [~1] pi #-}
  exp = map exp
  {-# INLINE [~1] exp #-}
  log = map log
  {-# INLINE [~1] log #-}
  sin = map sin
  {-# INLINE [~1] sin #-}
  cos = map cos
  {-# INLINE [~1] cos #-}
  asin = map asin
  {-# INLINE [~1] asin #-}
  atan = map atan
  {-# INLINE [~1] atan #-}
  acos = map acos
  {-# INLINE [~1] acos #-}
  sinh = map sinh
  {-# INLINE [~1] sinh #-}
  cosh = map cosh
  {-# INLINE [~1] cosh #-}
  asinh = map asinh
  {-# INLINE [~1] asinh #-}
  atanh = map atanh
  {-# INLINE [~1] atanh #-}
  acosh = map acosh
  {-# INLINE [~1] acosh #-}


-- Below is the simplistic, yet very powerful HIP fusion guts:

computeI :: ColorSpace cs e => Array A.D Ix2 (Pixel cs e) -> Image cs e
computeI = Image . A.compute
{-# INLINE [1] computeI #-}

delayI :: ColorSpace cs e => Image cs e -> Array A.D Ix2 (Pixel cs e)
delayI (Image arr) = A.delay arr
{-# INLINE [1] delayI #-}

{-# RULES
"delayI/computeI" forall arr . delayI (computeI arr) = arr
 #-}


-- INVESTIGATE: Does `dims` break fusion. If so, make a copy of size in `Image`.

dims :: ColorSpace cs e => Image cs e -> Ix2
dims (Image arr) = A.size arr
{-# INLINE dims #-}

scalar :: ColorSpace cs e => Pixel cs e -> Image cs e
scalar px = makeImage Seq (1 :. 1) (const px)
{-# INLINE [~1] scalar #-}

-- | Create an Image by supplying it's dimensions and a pixel generating function.
--
-- __Note__. If pixel generating function turns out to be a partially applied index function to
-- another image, this break fusuion and will cause that image to be fully computed. If that is not
-- what is desired, recommended approach is to use `traverse` instead.
makeImage :: ColorSpace cs e =>
             Comp -- ^ Computation strategy to use.
          -> Ix2 -- ^ (@m@ rows `:.` @n@ columns) - dimensions of a new image.
          -> (Ix2 -> Pixel cs e)
          -- ^ A function that takes (@i@-th row `:.` and @j@-th column) as an
          -- argument and returns a pixel for that location.
          -> Image cs e
makeImage comp sz = computeI . A.makeArray comp sz
{-# INLINE [~1] makeImage #-}


-- | Construct an image from a nested rectangular shaped list of pixels.
-- Length of an outer list will constitute @m@ rows, while the length of inner lists -
-- @n@ columns. All of the inner lists must be the same length and greater than @0@.
--
-- >>> fromLists Seq [[PixelY (fromIntegral (i*j) / 60000) | j <- [1..300]] | i <- [1..200]]
--
-- <<images/grad_fromLists.png>>
--
fromLists :: ColorSpace cs e =>
             Comp
          -> [[Pixel cs e]]
          -> Image cs e
fromLists comp = Image . A.fromLists' comp
{-# INLINE fromLists #-}

-- IO

-- | Display an image by making a call to an external viewer that is set as a default image viewer
-- by the OS.
displayImage :: (MonadIO m, ToRGBA cs e) => Image cs e -> m ()
displayImage (Image arr) = liftIO $ A.displayImage arr
{-# INLINE displayImage #-}


-- | Try to guess an image format from file's extension, then attempt to decode it as such. Color
-- space and precision of the result array must match exactly that of the actual image, in order to
-- apply auto conversion use `readImageAuto` instead.
--
-- Might throw `A.ConvertError`, `A.DecodeError` and other standard errors related to file IO.
--
-- Resulting image will be read as specified by the type signature:
--
-- >>> frog <- readImage "images/frog.jpg" :: IO (Image YCbCr Word8)
-- >>> displayImage frog
readImage ::
     (MonadIO m, ColorSpace cs e)
  => FilePath -- ^ File path for an image
  -> m (Image cs e)
readImage path = fmap Image (liftIO (A.readImage path))
{-# INLINE readImage #-}


-- | Same as `readImage`, but will perform any possible color space and
-- precision conversions in order to match the result image type. Very useful
-- whenever image format isn't known at compile time.
readImageAuto :: (MonadIO m, ColorSpace cs e) =>
                  FilePath -- ^ File path for an image
               -> m (Image cs e)
readImageAuto path = fmap Image (liftIO (A.readImageAuto path))
{-# INLINE readImageAuto #-}

-- | Inverse of the 'readImage', but similarly to it, will guess an output file format from the file
-- extension and will write to file any image with the colorspace that is supported by that
-- format. Precision of the image might be adjusted using `Elevator` whenever precision of the
-- source image is not supported by the image file format. For instance, <'Image' 'RGBA' 'Double'>
-- being saved as 'PNG' file would be written as <'Image' 'RGBA' 'Word16'>, thus using highest
-- supported precision 'Word16' for that format. If automatic colors space conversion is also
-- desired, `writeImageAuto` can be used instead.
--
-- Can throw `A.ConvertError`, `A.EncodeError` and other usual IO errors.
--
writeImage :: (MonadIO m, ColorSpace cs e) =>
               FilePath -> Image cs e -> m ()
writeImage path img = liftIO (A.writeImage path (delayI img))
{-# INLINE [~1] writeImage #-}



writeImageAuto ::
     (MonadIO m, ToYA cs e, ToRGBA cs e, ToYCbCr cs e, ToCMYK cs e)
  => FilePath
  -> Image cs e
  -> m ()
writeImageAuto path img = liftIO (A.writeImageAuto path (delayI img))
{-# INLINE [~1] writeImageAuto #-}


-- Indexing

-- | Get a pixel at @i@-th and @j@-th location.
--
-- >>> img = makeImage Seq (200 :. 200) (\(i :. j) -> PixelY $ fromIntegral (i*j)) / (200*200)
-- >>> index img (20 :. 30)
-- <Luma:(1.5e-2)>
--
index :: ColorSpace cs e => Image cs e -> Ix2 -> Pixel cs e
index (Image arr) = A.index' arr
{-# INLINE index #-}


-- | Infix synonym for `index`.
(!) :: ColorSpace cs e => Image cs e -> Ix2 -> Pixel cs e
(!) (Image arr) = A.index' arr
{-# INLINE (!) #-}


-- | Image indexing function that returns a default pixel if index is out of bounds.
defaultIndex :: ColorSpace cs e =>
                Pixel cs e -> Image cs e -> Ix2 -> Pixel cs e
defaultIndex px (Image arr) = A.defaultIndex px arr
{-# INLINE defaultIndex #-}


-- | Image indexing function that uses a special border resolutions strategy for
-- out of bounds pixels.
borderIndex :: ColorSpace cs e =>
               Border (Pixel cs e) -> Image cs e -> Ix2 -> Pixel cs e
borderIndex atBorder (Image arr) = A.borderIndex atBorder arr
{-# INLINE borderIndex #-}


-- | Image indexing function that returns @'Nothing'@ if index is out of bounds,
-- @'Just' px@ otherwise.
maybeIndex :: ColorSpace cs e =>
              Image cs e -> Ix2 -> Maybe (Pixel cs e)
maybeIndex (Image arr) = A.index arr
{-# INLINE maybeIndex #-}



-- Ops

-- | Map a function over a an image.
map :: (ColorSpace cs' e', ColorSpace cs e) =>
       (Pixel cs' e' -> Pixel cs e)
       -- ^ A function that takes a pixel of a source image and returns a pixel
       -- for the result image a the same location.
    -> Image cs' e' -- ^ Source image.
    -> Image cs e   -- ^ Result image.
map f = computeI . A.map f . delayI
{-# INLINE [~1] map #-}

-- | Map an index aware function over each pixel in an image.
imap :: (ColorSpace cs' e', ColorSpace cs e) =>
        (Ix2 -> Pixel cs' e' -> Pixel cs e)
      -- ^ A function that takes an index @(i, j)@, a pixel at that location
      -- and returns a new pixel at the same location for the result image.
     -> Image cs' e' -- ^ Source image.
     -> Image cs e   -- ^ Result image.
imap f = computeI . A.imap f . delayI
{-# INLINE [~1] imap #-}

-- | Zip two images with a function
zipWith :: (ColorSpace cs1 e1, ColorSpace cs2 e2, ColorSpace cs e) =>
           (Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs e)
        -> Image cs1 e1 -> Image cs2 e2 -> Image cs e
zipWith f img1 img2 = computeI $ A.zipWith f (delayI img1) (delayI img2)
{-# INLINE [~1] zipWith #-}

-- | Zip two images with an index aware function
izipWith :: (ColorSpace cs1 e1, ColorSpace cs2 e2, ColorSpace cs e) =>
            (Ix2 -> Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs e)
         -> Image cs1 e1 -> Image cs2 e2 -> Image cs e
izipWith f img1 img2 = computeI $ A.izipWith f (delayI img1) (delayI img2)
{-# INLINE [~1] izipWith #-}

-- | Zip three images with a function
zipWith3 :: (ColorSpace cs1 e1, ColorSpace cs2 e2, ColorSpace cs3 e3, ColorSpace cs e) =>
           (Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs3 e3 -> Pixel cs e)
        -> Image cs1 e1 -> Image cs2 e2 -> Image cs3 e3 -> Image cs e
zipWith3 f img1 img2 img3 = computeI $ A.zipWith3 f (delayI img1) (delayI img2) (delayI img3)
{-# INLINE [~1] zipWith3 #-}

-- | Zip three images with an index aware function
izipWith3 :: (ColorSpace cs1 e1, ColorSpace cs2 e2, ColorSpace cs3 e3, ColorSpace cs e) =>
            (Ix2 -> Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs3 e3 -> Pixel cs e)
         -> Image cs1 e1 -> Image cs2 e2 -> Image cs3 e3 -> Image cs e
izipWith3 f img1 img2 img3 = computeI $ A.izipWith3 f (delayI img1) (delayI img2) (delayI img3)
{-# INLINE [~1] izipWith3 #-}


-- | Transpose an image
transpose :: ColorSpace cs e => Image cs e -> Image cs e
transpose = computeI . A.transpose . delayI
{-# INLINE [~1] transpose #-}



-- | Traverse an image
traverse ::
     (ColorSpace cs' e', ColorSpace cs e)
  => (Ix2 -> Ix2) -- ^ Function that takes source image dimensions and returns dimensions of a new
                  -- image.
  -> ((Ix2 -> Pixel cs' e') -> Ix2 -> Pixel cs e)
  -- ^ Function that receives a pixel getter (a source image index function), a location @(i :. j)@
  -- in a new image and returns a pixel for that location.
  -> Image cs' e' -- ^ Source image.
  -> Image cs e
traverse fDim f = computeI . traverseArray fDim f . delayI
{-# INLINE [~1] traverse #-}

-- | Create an image, same as `traverse`, except by traversing two source images.
traverse2 ::
     (ColorSpace cs1 e1, ColorSpace cs2 e2, ColorSpace cs e)
  => (Ix2 -> Ix2 -> Ix2) -- ^ Function that returns dimensions of a new image.
  -> ((Ix2 -> Pixel cs1 e1) -> (Ix2 -> Pixel cs2 e2) -> Ix2 -> Pixel cs e)
         -- ^ Function that receives pixel getters, a location @(i :. j)@ in a new image and returns a
         -- pixel for that location.
  -> Image cs1 e1 -- ^ First source image.
  -> Image cs2 e2 -- ^ Second source image.
  -> Image cs e
traverse2 fDims f img1 img2 = computeI $ traverseArray2 fDims f (delayI img1) (delayI img2)
{-# INLINE [~1] traverse2 #-}

-- Folding

-- | Undirected reduction of an image.
fold :: ColorSpace cs e =>
        (Pixel cs e -> Pixel cs e -> Pixel cs e) -- ^ An associative folding function.
     -> Pixel cs e -- ^ Initial element, that is neutral with respect to the folding function.
     -> Image cs e -- ^ Source image.
     -> Pixel cs e
fold f acc = A.fold f acc . delayI
{-# INLINE [~1] fold #-}

-- | Monoidal reduction of an image.
foldMono ::
     (Monoid m, ColorSpace cs e)
  => (Pixel cs e -> m) -- ^ Function that converts every pixel in the image to a `Monoid`.
  -> Image cs e -- ^ Source image.
  -> m
foldMono f = A.fold (<>) mempty . A.map f . delayI
{-# INLINE [~1] foldMono #-}


-- Lifting


-- | Zip two images with a function
liftImage2 :: (ColorSpace cs1 e1, ColorSpace cs2 e2, ColorSpace cs e) =>
           (Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs e)
        -> Image cs1 e1 -> Image cs2 e2 -> Image cs e
liftImage2 f img1 img2 = computeI $ liftArray2 f (delayI img1) (delayI img2)
{-# INLINE [~1] liftImage2 #-}


-- Array functions.

-- | Create an array by traversing a source array.
traverseArray ::
     (Source r1 ix1 e1, Index ix)
  => (ix1 -> ix)
  -> ((ix1 -> e1) -> ix -> e)
  -> Array r1 ix1 e1
  -> Array A.D ix e
traverseArray fSz f arr = A.makeArray (A.getComp arr) (fSz (A.size arr)) (f (A.evaluateAt arr))
{-# INLINE traverseArray #-}

-- | Create an array, same as `traverseArray`, except by traversing two source arrays.
traverseArray2
  :: (Source r1 ix1 e1, Source r2 ix2 e2, Index ix)
  => (ix1 -> ix2 -> ix)
  -> ((ix1 -> e1) -> (ix2 -> e2) -> ix -> e)
  -> Array r1 ix1 e1
  -> Array r2 ix2 e2
  -> Array A.D ix e
traverseArray2 fSz f arr1 arr2 =
  A.makeArray
    (A.getComp arr1)
    (fSz (A.size arr1) (A.size arr2))
    (f (A.evaluateAt arr1) (A.evaluateAt arr2))
{-# INLINE traverseArray2 #-}


-- | TODO: expose liftArray2 as internal in massiv.
liftArray2
  :: (Source r1 ix a, Source r2 ix b)
  => (a -> b -> e) -> Array r1 ix a -> Array r2 ix b -> Array A.D ix e
liftArray2 f arr1 arr2
  | sz1 == oneIndex = A.map (f (unsafeIndex arr1 zeroIndex)) arr2
  | sz2 == oneIndex = A.map (`f` (unsafeIndex arr2 zeroIndex)) arr1
  | sz1 == sz2 =
    A.makeArrayR A.D (A.getComp arr1) sz1 (\ ix -> f (unsafeIndex arr1 ix) (unsafeIndex arr2 ix))
  | otherwise =
    error $
    "Array dimensions must be the same, instead got: " ++
    show sz1 ++ " and " ++ show sz2
  where
    oneIndex = pureIndex 1
    sz1 = A.size arr1
    sz2 = A.size arr2
{-# INLINE liftArray2 #-}
