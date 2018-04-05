{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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
  , makeImageC
  , fromArray
  , toArray
  , getComp
  , setComp
  , dims
  , (!)
  , index
  , maybeIndex
  , defaultIndex
  , borderIndex
  , map
  , imap
  , zipWith
  , zipWith3
  , izipWith
  , izipWith3
  , traverse
  , traverse2
  , transpose
  , fold
  , foldMono
  ) where

import qualified Data.Massiv.Array        as A
import           Data.Massiv.Array.Unsafe as A
import           Data.Massiv.Core
import           Data.Monoid
import           Data.Typeable
import           Graphics.ColorSpace
import           Prelude                  as P hiding (map, zipWith, zipWith3, traverse)


-- | Main data type of the library
data Image cs e = Image !(Array A.S Ix2 (Pixel cs e))
-- It is not a newtype, just so the fusion would work properly

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


dims :: ColorSpace cs e => Image cs e -> Ix2
dims = A.size . delayI
{-# INLINE [~1] dims #-}
-- INVESTIGATE: Does `dims` break fusion. If so, check if making a copy of it in `Image` alleviates
-- the issue, otherwise document it.
-- CHECK if this approach is better: dims (Image arr) = A.size arr

-- | By default all images are created with parallel computation strategy, but it can be changed
-- with this function.
setComp :: ColorSpace cs e => Comp -> Image cs e -> Image cs e
setComp comp = computeI . A.setComp comp . delayI
{-# INLINE [~1] setComp #-}

getComp :: ColorSpace cs e => Image cs e -> Comp
getComp = A.getComp . delayI
{-# INLINE [~1] getComp #-}

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
makeImageC :: ColorSpace cs e =>
              Comp
           -> Ix2 -- ^ (@m@ rows `:.` @n@ columns) - dimensions of a new image.
           -> (Ix2 -> Pixel cs e)
           -- ^ A function that takes (@i@-th row `:.` and @j@-th column) as an
           -- argument and returns a pixel for that location.
          -> Image cs e
makeImageC comp sz = computeI . A.makeArray comp sz
{-# INLINE [~1] makeImageC #-}

-- | Convert a 2-dimensional source array of pixels into an image.
fromArray :: (Source r Ix2 (Pixel cs e), ColorSpace cs e) => Array r Ix2 (Pixel cs e) -> Image cs e
fromArray = Image . A.computeSource
{-# INLINE fromArray #-}

-- | Convert an image into a storable array of pixels
toArray :: Image cs e -> Array A.S Ix2 (Pixel cs e)
toArray (Image arr) = arr
{-# INLINE toArray #-}

-- Indexing

-- | Get a pixel at @i@-th and @j@-th location.
--
-- >>> img = makeImage (200 :. 200) (\(i :. j) -> PixelY $ fromIntegral (i*j)) / (200*200)
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
