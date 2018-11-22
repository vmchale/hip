{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Graphics.Image.Internal
-- Copyright   : (c) Alexey Kuleshevich 2016-2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Internal
  ( Image(..)
  , Ix2(..)
  , Comp(..)
  , ColorSpace(..)
  , Pixel
  , Border(..)
  , computeI
  , delayI
  , makeImage
  , makeImageComp
  , fromLists
  , toLists
  , fromArray
  , toArray
  , setComp
  , dims
  , isEmpty
  , totalPixels
  , map
  , imap
  , zipWith
  , zipWith3
  , izipWith
  , izipWith3
  , traverse
  , traverse2
  , transmute
  , transmute2
  , backpermute
  -- , transmute3
  , fold
  , foldMono
  , foldSemi
  , foldSemi1
  , maxPixel
  , minPixel
  , maxVal
  , minVal
  ) where

import           Control.DeepSeq
import qualified Data.Massiv.Array        as A
import           Data.Massiv.Core         hiding (isEmpty)
import           Data.Semigroup
import           Data.Typeable
import           GHC.Exts                 (IsList (..))
import           Graphics.ColorSpace
import           Prelude                  as P hiding (map, traverse, zipWith,
                                                       zipWith3)

-- | Main data type of the library
data Image cs e = Image !(Array A.S Ix2 (Pixel cs e))
-- It is not a newtype, just so the fusion works properly

instance ColorSpace cs e => Show (Image cs e) where
  show img =
    let (m :. n) = dims img
    in "<Image " ++
       showsTypeRep (typeRep (Proxy :: Proxy cs)) " " ++
       showsTypeRep (typeRep (Proxy :: Proxy e)) ": " ++ show m ++ "x" ++ show n ++ ">"

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


instance (NFData e, ColorSpace cs e) => NFData (Image cs e) where
  rnf (Image arr) = rnf arr

instance ColorSpace cs e => IsList (Image cs e) where
  type Item (Image cs e) = [Pixel cs e]
  fromList = fromLists
  toList = toLists

-- Below is very simplistic, yet verextremely powerful HIP fusion guts:

computeI :: ColorSpace cs e => Array A.D Ix2 (Pixel cs e) -> Image cs e
computeI = Image . A.compute
{-# INLINE [1] computeI #-}

delayI :: ColorSpace cs e => Image cs e -> Array A.D Ix2 (Pixel cs e)
delayI (Image arr) = A.delay arr
{-# INLINE [1] delayI #-}

{-# RULES
"fuse delayI/computeI" [~1] forall arr . delayI (computeI arr) = arr
 #-}

-- | Get image dimensions. Using this function will cause an image to be fully evaluated and will
-- break the fusion at the call site, so it is recommended to avoid it in favor of `traverse` when
-- possible.
dims :: ColorSpace cs e => Image cs e -> Ix2
dims (Image arr) = A.size arr
{-# INLINE dims #-}

-- | Check if image is empty, i.e. at least one of its sides is equal to 0. Uses `dims` underneath.
isEmpty :: ColorSpace cs e => Image cs e -> Bool
isEmpty (Image arr) = A.isEmpty arr
{-# INLINE isEmpty #-}

totalPixels :: ColorSpace cs e => Image cs e -> Int
totalPixels = A.totalElem . dims
{-# INLINE totalPixels #-}

-- | By default all images are created with parallel computation strategy, but it can be changed
-- with this function.
setComp :: ColorSpace cs e => Comp -> Image cs e -> Image cs e
setComp comp = computeI . A.setComp comp . delayI
{-# INLINE [~1] setComp #-}


-- | Create a scalar image with only one element. Could be handy together with `liftArray2`
-- function.
scalar :: ColorSpace cs e => Pixel cs e -> Image cs e
scalar px = computeI $ A.makeArray Seq (1 :. 1) (const px)
{-# INLINE [~1] scalar #-}

-- | Create an Image by supplying it's dimensions and a pixel generating function.
--
-- __Note__. If another image(s) is being used to make the new one with this function, it will
-- likely to be breaking fusion and will cause that source image to be fully computed. It is
-- recommended to use `traverse` or `traverse2` instead.
makeImage :: ColorSpace cs e =>
             Ix2 -- ^ (@m@ rows `:.` @n@ columns) - dimensions of a new image.
          -> (Ix2 -> Pixel cs e)
          -- ^ A function that takes (@i@-th row `:.` and @j@-th column) as an
          -- argument and returns a pixel for that location.
          -> Image cs e
makeImage sz = computeI . A.makeArray Par sz
{-# INLINE [~1] makeImage #-}

-- | Same as `makeImage`, except computation startegy can be supplied as an argument.
makeImageComp :: ColorSpace cs e =>
                 Comp
              -> Ix2 -- ^ (@m@ rows `:.` @n@ columns) - dimensions of a new image.
              -> (Ix2 -> Pixel cs e)
              -- ^ A function that takes (@i@-th row `:.` and @j@-th column) as an
              -- argument and returns a pixel for that location.
             -> Image cs e
makeImageComp comp sz = computeI . A.makeArray comp sz
{-# INLINE [~1] makeImageComp #-}

-- | Convert a 2-dimensional source array of pixels into an image.
fromArray :: (Source r Ix2 (Pixel cs e), ColorSpace cs e) => Array r Ix2 (Pixel cs e) -> Image cs e
fromArray = Image . A.computeSource
{-# INLINE fromArray #-}

-- | Convert an image into a storable array of pixels
toArray :: Image cs e -> Array A.S Ix2 (Pixel cs e)
toArray (Image arr) = arr
{-# INLINE toArray #-}


-- | Construct an image from a nested rectangular shaped list of pixels.  Length of an outer list
-- will constitute @m@ rows, while the length of inner lists - @n@ columns. All of the inner lists
-- must be of the same length, otherwise an error will be thrown.
--
-- >>> fromLists [[PixelY (fromIntegral (i*j) / 60000) | j <- [1..300]] | i <- [1..200]]
--
-- <<images/grad_fromLists.png>>
--
fromLists :: ColorSpace cs e => [[Pixel cs e]] -> Image cs e
fromLists = Image . A.fromLists' Par
{-# INLINE fromLists #-}


-- | Convert an image into a nested lists of pixels
--
-- prop> img == fromLists (toLists img)
toLists :: ColorSpace cs e => Image cs e -> [[Pixel cs e]]
toLists (Image arr) = A.toLists arr
{-# INLINE toLists #-}




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


-- | Sort of like `traverse`, but there is ability to use result of size generating function inside
-- of the element generating function
transmute ::
     (ColorSpace cs' e', ColorSpace cs e)
  => (Ix2 -> (Ix2, a)) -- ^ Function that takes source image dimensions and returns dimensions of a
                  -- new image as well as anything else that will be available to an indexing
                  -- function.
  -> (a -> (Ix2 -> Pixel cs' e') -> Ix2 -> Pixel cs e)
  -- ^ Function that receives a pixel getter (a source image index function), a location @(i :. j)@
  -- in a new image and returns a pixel for that location.
  -> Image cs' e' -- ^ Source image.
  -> Image cs e
transmute fDim f = computeI . transmuteArray fDim f . delayI
{-# INLINE [~1] transmute #-}

-- | Create an image, same as `transmute`, except by working on two source images.
transmute2 ::
     (ColorSpace cs1 e1, ColorSpace cs2 e2, ColorSpace cs e)
  => (Ix2 -> Ix2 -> (Ix2, a)) -- ^ Function that returns dimensions of a new image and other data
                              -- for indexing function.
  -> (a -> (Ix2 -> Pixel cs1 e1) -> (Ix2 -> Pixel cs2 e2) -> Ix2 -> Pixel cs e)
         -- ^ Function that receives pixel getters, a location @(i :. j)@ in a new image and returns a
         -- pixel for that location.
  -> Image cs1 e1 -- ^ First source image.
  -> Image cs2 e2 -- ^ Second source image.
  -> Image cs e
transmute2 fDims f img1 img2 = computeI $ transmuteArray2 fDims f (delayI img1) (delayI img2)
{-# INLINE [~1] transmute2 #-}

-- | Sort of like `traverse`, but there is ability to use result of size generating function inside
-- of the element generating function
backpermute ::
     (ColorSpace cs e)
  => (Ix2 -> (Ix2, a))
  -- ^ Function that takes source image dimensions and returns dimensions of a new image as well as
  -- anything else that will be available to an indexing function.
  -> (a -> Ix2 -> Ix2)
  -- ^ Function that maps an index of a result image to an index of a source image.
  -> Image cs e -- ^ Source image.
  -> Image cs e
backpermute fDim f =
  computeI .
  (\arr ->
     let (sz, a) = fDim (A.size arr)
     in A.backpermute sz (f a) arr) .
  delayI
{-# INLINE [~1] backpermute #-}


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
foldMono f = A.fold mappend mempty . A.map f . delayI
{-# INLINE [~1] foldMono #-}


-- | Semigroup reduction of an image.
foldSemi ::
     (Semigroup m, ColorSpace cs e)
  => (Pixel cs e -> m) -- ^ Function that converts every pixel in the image to a `Semigroup`.
  -> m -- ^ Initial Element
  -> Image cs e -- ^ Source image.
  -> m
foldSemi f m = A.fold (<>) m . A.map f . delayI
{-# INLINE [~1] foldSemi #-}

-- FIX: initial element will get counted twice. Either implement fold1 upstream or do `(extract 1
-- (sz-1) . resize (totalElem sz))`. Benchmark the affect of adjustment.
-- | Semigroup reduction of a non-empty image, while using the 0th pixel as initial element. Will
-- throw an index out of bounds error if image is empty.
foldSemi1 ::
     (Semigroup m, ColorSpace cs e)
  => (Pixel cs e -> m) -- ^ Function that converts every pixel in the image to a `Semigroup`.
  -> Image cs e -- ^ Source image.
  -> m
foldSemi1 f = (\arr -> A.fold (<>) (f (A.evaluateAt arr 0)) (A.map f arr)) . delayI
{-# INLINE [~1] foldSemi1 #-}

-- | Find the largest pixel. Throws an error on empty (see `isEmpty`) images.
maxPixel ::
     (Ord (Pixel cs e), ColorSpace cs e)
  => Image cs e -- ^ Source image.
  -> Pixel cs e
maxPixel = getMax . foldSemi1 Max
{-# INLINE [~1] maxPixel #-}

-- | Find the largest pixel. Throws an error on empty (see `isEmpty`) images.
minPixel ::
     (Ord (Pixel cs e), ColorSpace cs e)
  => Image cs e -- ^ Source image.
  -> Pixel cs e
minPixel = getMin . foldSemi1 Min
{-# INLINE [~1] minPixel #-}

-- | Find the largest channel value among all pixels in the image. Throws an error on empty (see
-- `isEmpty`) images.
maxVal :: (Ord e, ColorSpace cs e) => Image cs e -> e
maxVal = (A.maximum . A.map maximum) . delayI
{-# INLINE [~1] maxVal #-}

-- | Find the smallest channel value among all pixels in the image. Throws an error on empty (see
-- `isEmpty`) images.
minVal :: (Ord e, ColorSpace cs e) => Image cs e -> e
minVal = (A.minimum . A.map minimum) . delayI
{-# INLINE [~1] minVal #-}


-- Lifting


-- | Zip two images with a function
liftImage2 :: (ColorSpace cs1 e1, ColorSpace cs2 e2, ColorSpace cs e) =>
           (Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs e)
        -> Image cs1 e1 -> Image cs2 e2 -> Image cs e
liftImage2 f img1 img2 = computeI $ A.liftArray2 f (delayI img1) (delayI img2)
{-# INLINE [~1] liftImage2 #-}

-- Array functions.

-- | Create a new array by using size and indexing function of a source array.
transmuteArray ::
     (Source r Ix2 e1)
  => (Ix2 -> (Ix2, a))
  -> (a -> (Ix2 -> e1) -> Ix2 -> e)
  -> Array r Ix2 e1
  -> Array A.D Ix2 e
transmuteArray fSz f arr = A.makeArray (A.getComp arr) sz (f a (A.evaluateAt arr))
  where (sz, a) = fSz (A.size arr)
{-# INLINE transmuteArray #-}


-- | Create an array, same as `transmuteArray`, except by traversing two source arrays.
transmuteArray2
  :: (Source r1 Ix2 e1, Source r2 Ix2 e2)
  => (Ix2 -> Ix2 -> (Ix2, a))
  -> (a -> (Ix2 -> e1) -> (Ix2 -> e2) -> Ix2 -> e)
  -> Array r1 Ix2 e1
  -> Array r2 Ix2 e2
  -> Array A.D Ix2 e
transmuteArray2 fSz f arr1 arr2 =
  A.makeArray
    (A.getComp arr1)
    sz
    (f a (A.evaluateAt arr1) (A.evaluateAt arr2))
  where
    (sz, a) = fSz (A.size arr1) (A.size arr2)
{-# INLINE transmuteArray2 #-}


-- | Create an array by traversing a source array.
traverseArray ::
     (Source r1 Ix2 e1)
  => (Ix2 -> Ix2)
  -> ((Ix2 -> e1) -> Ix2 -> e)
  -> Array r1 Ix2 e1
  -> Array A.D Ix2 e
traverseArray fSz f = transmuteArray (\sz -> (fSz sz, ())) (const f)
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
