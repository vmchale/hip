module Main where

import           Criterion.Main
import           Graphics.Image                   as I
import           Graphics.Image.Processing.Filter
import           Prelude                          as P




main :: IO ()
main = do
  imgRGB8 <- readImageAuto "images/frog.jpg" :: IO (Image RGB Word8)
  --imgRGB64 <- readImageAuto "images/frog.jpg" :: IO (Image RGB Word8)
  defaultMain
    [ env (return imgRGB8) $ \img ->
        bgroup
          "Sobel Horizontal RGB8"
          [ bench "No Normalization" $ whnf (applyFilter Edge sobelHorizontal) img
          , bench "Smart Normalization" $ whnf (applyFilter Edge sobelHorizontalN) img
          ]
    , env (return (I.map toDouble imgRGB8)) $ \img ->
        bgroup
          "Sobel Horizontal RGBD"
          [ bench "No Normalization" $ whnf (applyFilter Edge sobelHorizontal) img
          , bench "Smart Normalization" $ whnf (applyFilter Edge sobelHorizontalN) img
          , bench "Generic Normalization" $ whnf (normalize . applyFilter Edge sobelHorizontal) img
          ]
    , env (return imgRGB8) $ \img ->
        bgroup
          "Sobel Operator RGB8"
          [ bench "No Normalization" $ whnf (applyFilter Edge sobelOperator . I.map toDouble) img
          , bench "Smart Normalization" $ whnf (applyFilter Edge sobelOperatorN . I.map toDouble) img
          , bench "Generic Normalization" $ whnf (normalize . applyFilter Edge sobelOperator . I.map toDouble) img
          ]
    , env (return (I.map toDouble imgRGB8)) $ \img ->
        bgroup
          "Sobel Operator RGBD"
          [ bench "No Normalization" $ whnf (applyFilter Edge sobelOperator) img
          , bench "Smart Normalization" $ whnf (applyFilter Edge sobelOperatorN) img
          , bench "Generic Normalization" $ whnf (normalize . applyFilter Edge sobelOperator) img
          ]
    -- , env (return imgRGB8) $ \img ->
    --     bgroup
    --       "Laplacian RGB8"
    --       [ bench "Elevated'" $ whnf (applyFilter Edge laplacian) img
    --       , bench "Elevated" $ whnf (applyFilter Edge laplacianFilterElevated) img
    --       , bench "Rewrite" $ whnf (applyFilter Edge laplacianFilter) img
    --       ]
    -- , env (return (I.map toDouble imgRGB8)) $ \img ->
    --     bgroup
    --       "Laplacian RGBD"
    --       [ bench "Elevated'" $ whnf (applyFilter Edge laplacian) img
    --       , bench "Elevated" $ whnf (applyFilter Edge laplacianFilterElevated) img
    --       , bench "Rewrite" $ whnf (applyFilter Edge laplacianFilter) img
    --       ]
    ]
