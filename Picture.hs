module Picture where

import Data.List (transpose,tails)

-- Data Structures for representing an Picture
-- A Pixel is just a triple of doubles between 0 and 1
-- let x = Pixel r g b
-- is a pixel with a red greed and blue component.
-- A Picture is a 2D list of pixels.

data Pixel = Pixel Double Double Double
 deriving Show
type Picture = [[Pixel]]

-- A large pixel (used for pixelate)
type BigPixel = [[Pixel]]

-- A single black pixel
black :: Pixel
black = Pixel 0 0 0

-----------------------------------------------------------------------------
--
-- Assignment 4: A basic image library
--
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
--
-- Part 1: useful functions
--
-----------------------------------------------------------------------------

-- A function to scale a pixel by a given number
pixelScale :: Double -> Pixel -> Pixel
pixelScale factor (Pixel r g b) = Pixel (factor * r) (factor * g) (factor * b)

-- Add 2 pixels together componentwise
pixelAdd :: Pixel -> Pixel -> Pixel
pixelAdd (Pixel r1 g1 b1) (Pixel r2 g2 b2) = Pixel (r1 + r2) (g1 + g2) (b1 + b2)

-- get the red component of a pixel
red :: Pixel -> Double
red (Pixel r _ _) = r

-- get the green component of a pixel
green :: Pixel -> Double
green (Pixel _ g _) = g

-- get the blue component of a pixel
blue :: Pixel -> Double
blue (Pixel _ _ b) = b

-- A function that takes a pixel transformation
-- and applies it to all of the pixels in the image

-- THIS ONE
picMap :: (a -> a) -> [[a]] -> [[a]]
picMap f = map (map f)

-- group a list into groups of size n.
-- example group 2 [123456]
-- [[12][34][56]
group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = take n xs : group n (drop n xs)

-- returns the height of an image
height :: [[a]] -> Int
height = length

-- returns the width of an image
-- If an image has no rows then it should have a width of 0
width :: [[a]] -> Int
width image
    | null image  = 0
    | otherwise   = length (head image)

-- creates an NxM matrix of black pixels
-- N rows
-- M columns
blackBox :: Int -> Int -> [[Pixel]]
blackBox rows cols = replicate rows (replicate cols blackPixel)
    where
        blackPixel = black

-- adds n rows of black pixels to the top of the image
padTop :: Int -> Picture -> Picture
padTop n image = blackRows ++ image
    where
        blackRows = replicate n (replicate (width image) blackPixel)
        blackPixel = Pixel 0 0 0

-- adds n rows of black pixels to the bottom of the image
padBottom :: Int -> Picture -> Picture
padBottom n image = image ++ blackRows
    where
        blackRows = replicate n (replicate (width image) blackPixel)
        blackPixel = Pixel 0 0 0

-- adds n rows of black pixels to the left of the image
padLeft :: Int -> Picture -> Picture
padLeft n = map (replicate n blackPixel ++)
    where
        blackPixel = Pixel 0 0 0

-- adds n rows of black pixels to the right of the image
padRight :: Int -> Picture -> Picture
padRight n = map (++ replicate n blackPixel)
    where
        blackPixel = Pixel 0 0 0

-- pad an immage to the left and the right with n columns of black pixels.
padH :: Int -> Picture -> Picture
padH n image = padRight n (padLeft n image)

-- pad an immage above and below with n rows of black pixels
padV :: Int -> Picture -> Picture
padV n image = padBottom n (padTop n image)

-- cell shades an image
cellShade :: Picture -> Picture
cellShade = map (map toCellShade)
  where
    toCellShade :: Pixel -> Pixel
    toCellShade (Pixel r g b) = let
      cellShadeValue x = fromIntegral (round (x * 3)) / 3
      in Pixel (cellShadeValue r) (cellShadeValue g) (cellShadeValue b)


-- converts an image to gray scale.
grayScale :: Picture -> Picture
grayScale = map (map grayPixel)
  where
    grayPixel (Pixel r g b) = let avg = (r + g + b) / 3 in Pixel avg avg avg


--------------------------------------------------------------------------
--
-- Assignment 8
--
--------------------------------------------------------------------------

-- compute the average color of a list of pixels
average :: [Pixel] -> Pixel
average = undefined

-- put p2 below p1.
-- If the two pictures are not the same width
-- you will need to add black space to the right of the smaller picture
addDown :: Picture -> Picture -> Picture
addDown = undefined

-- put p2 above p1.
-- If the two pictures are not the same width
-- you will need to add black space to the right of the smaller picture
addUp :: Picture -> Picture -> Picture
addUp = undefined

-- put p2 to the right of p1.
-- If the two pictures are not the same height
-- you will need to add black space below the smaller picture
addRight :: Picture -> Picture -> Picture
addRight = undefined

-- put p2 to the left of p1.
-- If the two pictures are not the same height
-- you will need to add black space below the smaller picture
addLeft :: Picture -> Picture -> Picture
addLeft = undefined

-- these two functions (and transpose from the List library)
-- are all you need to complete the flip and rotate functions.
-- Every other function can be done with a composition of these three functions.
-- To figure out how get a paper square and draw A B C D on the corners
--
-- -------------------
-- |A               B|
-- |                 |
-- |                 |
-- |                 |
-- |                 |
-- |                 |
-- |D               C|
-- -------------------
--
-- Now flip the square around using horizontal virtical or transpose flips
-- you can recreate any of the orther translations!

-- flip the image horizontally (mirror the image)
flipH :: [[a]] -> [[a]]
flipH = undefined

-- flip the image vertically
flipV :: [[a]] -> [[a]]
flipV = undefined

-- rotate an image 90 degrees
rot90 :: [[a]] -> [[a]]
rot90 = undefined

-- rotate the image 180 degrees
rot180 :: [[a]] -> [[a]]
rot180 = undefined

-- rotate the image 270 degrees
rot270 :: [[a]] -> [[a]]
rot270 = undefined

-- flip the image over the first diagonal
-- this is the diagonal from A to C
flip1 :: [[a]] -> [[a]]
flip1 = undefined

-- flip the image over the second diagonal
-- this is the diagonal from B to D
flip2 :: [[a]] -> [[a]]
flip2 = undefined


pixelate :: Int -> Picture -> Picture
pixelate = undefined

-- really this is Int -> Picture -> [[BigPixel]]
makePixels :: Int -> [[a]] -> [[[[a]]]]
makePixels = undefined

-- really this is [[BigPixel]] -> Picture
unmakePixels :: [[[[a]]]] -> [[a]]
unmakePixels = undefined

sameColor :: Int -> BigPixel -> BigPixel
sameColor = undefined


---------------------------------------------------------------------------
--
-- Extra Credit: Convolution
--
---------------------------------------------------------------------------


matrixZipWith :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
matrixZipWith = undefined

matrixFold :: (a -> a -> a) -> a -> [[a]] -> a
matrixFold = undefined

convolve :: (b -> b -> b) -> (a -> b -> b) -> b -> [[a]] -> [[b]] -> b
convolve = undefined

-- takes a kernel and a pixel of the same size (a square matrix)
-- and multiplies each element pointwise.
-- We then add all of the pixels together to get a single pixel.
convolvePic :: [[Double]] -> [[Pixel]] -> Pixel
convolvePic = undefined

-- get a list of sliding windows from a list
-- example: 
-- windows 3 [123456]
-- gives us
-- [[123][234][345][456]]
windows :: Int -> [a] -> [[a]]
windows = undefined

convolveImage :: [[Double]] -> [[Pixel]] -> [[Pixel]]
convolveImage = undefined

-- edge detections
-- The edged tend to come out pretty faint so I scale up everything in the
-- image by a factor of 4.
edge :: Picture -> Picture
edge = picMap (pixelScale 4) . convolveImage kernal . grayScale
 where kernal = [[ 0-1 0]
                 [-1 4-1]
                 [ 0-1 0]]

-- blur the image using a flat blurring.
-- This tends to blur a little more but it doesn't look as good as gausing bluring.
blur :: Picture -> Picture
blur = convolveImage kernal
 where kernal = [[1/251/251/251/251/25] 
                 [1/251/251/251/251/25] 
                 [1/251/251/251/251/25] 
                 [1/251/251/251/251/25] 
                 [1/251/251/251/251/25]]

-- gaussian bluring
-- blur each pixel with a weighted average of all the pixels around it.
-- The weights come from a gaussian distributaion
-- (technically a binomial distribution since pictures are discrete)
gaussian :: Picture -> Picture
gaussian = convolveImage kernal
 where kernal = [[1/273 4/273 7/273 4/2731/273] 
                 [4/27316/27327/27316/2734/273] 
                 [7/27326/27341/27326/2737/273] 
                 [4/27316/27327/27316/2734/273] 
                 [1/273 4/273 7/273 4/2731/273]]

