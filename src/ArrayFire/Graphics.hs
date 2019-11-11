{-# LANGUAGE ViewPatterns #-}
--------------------------------------------------------------------------------
-- |
-- Module      : ArrayFire.Graphics
-- Copyright   : David Johnson (c) 2019-2020
-- License     : BSD 3
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
-- Functions for displaying 'Array' graphically.
--
-- @
-- >>> window <- createWindow 800 600 "New Chart"
-- @
--
--------------------------------------------------------------------------------
module ArrayFire.Graphics where

import Control.Exception
import Foreign.Marshal
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.C.String

import ArrayFire.Internal.Graphics
import ArrayFire.Exception
import ArrayFire.FFI
import ArrayFire.Internal.Types

-- | Create window
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__window.htm)
--
-- >>> window <- createWindow 800 600 "New Chart"
--
createWindow
  :: Int
  -- ^ width
  -> Int
  -- ^ height
  -> String
  -- ^ title
  -> IO Window
  -- ^ 'Window' handle
createWindow (fromIntegral -> x) (fromIntegral -> y) str =
  withCString str $ \cstr ->
    createWindow' (\p -> af_create_window p x y cstr)

-- | Sets 'Window' position
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__window.htm)
--
-- >>> window <- createWindow 800 600 "New Chart"
-- >>> setPosition window 800 600
--
setPosition
  :: Window
  -- ^ 'Window' handle
  -> Int
  -- ^ Horizontal start coordinate
  -> Int
  -- ^ Vertical start coordinate
  -> IO ()
setPosition w (fromIntegral -> x) (fromIntegral -> y) =
  w `opw` (\p -> af_set_position p x y)

-- | Sets 'Window' title
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__window.htm)
--
-- >>> window <- createWindow 800 600 "New Chart"
-- >>> setTitle window "window title"
--
setTitle
  :: Window
  -- ^ 'Window' handle
  -> String
  -- ^ title
  -> IO ()
setTitle w str = withCString str $ \cstr ->
  w `opw` (\p -> af_set_title p cstr)

-- | Sets 'Window' size
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__window.htm)
--
-- >>> window <- createWindow 800 600 "New Chart"
-- >>> setSize window 800 600
--
setSize
  :: Window
  -- ^ 'Window' handle
  -> Int
  -- ^ target width of the window
  -> Int
  -- ^ target height of the window
  -> IO ()
setSize w (fromIntegral -> x) (fromIntegral -> y) =
  w `opw` (\p -> af_set_size p x y)

-- | Draw an image onto a Window
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__draw.htm)
--
-- @
-- main :: IO ()
-- main = render =<< createWindow 800 600 "hey"
--     where
--       render window = do
--         let x = iota [1,60] [60,1] / (30 :: Array Float)
--             cell = Cell (-1) (-1) "" ColorMapDefault
--         drawImage window x cell
--         closed <- isWindowClosed window
--         unless closed (render window )
-- @
drawImage
  :: Window
  -- ^ 'Window' handle
  -> Array a
  -- ^ Image
  -> Cell
  -- ^ is structure 'Cell' that has the properties that are used for the current rendering.
  -> IO ()
drawImage (Window wfptr) (Array fptr) cell =
  mask_ $ withForeignPtr fptr $ \aptr ->
    withForeignPtr wfptr $ \wptr ->
      alloca $ \cellPtr -> do
        poke cellPtr =<< cellToAFCell cell
        throwAFError =<< af_draw_image wptr aptr cellPtr

-- | Draw a plot onto a 'Window'
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__draw.htm)
--
-- >>> drawPlot window ('constant' \@'Int' 1) ('constant' \@'Int' 1) ('Cell' 10 10 "test" 'ColorMapSpectrum')
--
-- *Note* X and Y should be vectors.
--
drawPlot
  :: Window
  -- ^ is the window handle
  -> Array a
  -- ^ is an 'Array' with the x-axis data points
  -> Array a
  -- ^ is an 'Array' with the y-axis data points
  -> Cell
  -- ^ is structure 'Cell' that has the properties that are used for the current rendering.
  -> IO ()
drawPlot (Window w) (Array fptr1) (Array fptr2) cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
    withForeignPtr fptr2 $ \ptr2 ->
      withForeignPtr w $ \wptr ->
      alloca $ \cellPtr -> do
        poke cellPtr =<< cellToAFCell cell
        throwAFError =<< af_draw_plot wptr ptr1 ptr2 cellPtr

-- | Draw a plot onto a 'Window'
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__draw.htm)
--
-- *Note* P should be a 3n x 1 vector or one of a 3xn or nx3 matrices.
--
drawPlot3
  :: Window
  -- ^ the window handle
  -> Array a
  -- ^ is an af_array or matrix with the xyz-values of the points
  -> Cell
  -- ^ is structure af_cell that has the properties that are used for the current rendering.
  -> IO ()
drawPlot3 (Window w) (Array fptr) cell =
  mask_ $ withForeignPtr fptr $ \aptr ->
    withForeignPtr w $ \wptr ->
    alloca $ \cellPtr -> do
      poke cellPtr =<< cellToAFCell cell
      throwAFError =<< af_draw_plot3 wptr aptr cellPtr

-- | Draw a plot onto a 'Window'
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__draw.htm)
--
-- *Note* in must be 2d and of the form [n, order], where order is either 2 or 3. If order is 2, then chart is 2D and if order is 3, then chart is 3D.
--
drawPlotNd
  :: Window
  -- ^ is the window handle
  -> Array a
  -- ^ is an 'Array' or matrix with the xyz-values of the points
  -> Cell
  -- ^ is structure 'Cell' that has the properties that are used for the current rendering.
  -> IO ()
drawPlotNd (Window w) (Array fptr) cell =
  mask_ $ withForeignPtr fptr $ \aptr ->
    withForeignPtr w $ \wptr ->
    alloca $ \cellPtr -> do
      poke cellPtr =<< cellToAFCell cell
      throwAFError =<< af_draw_plot_nd wptr aptr cellPtr

-- | Draw a plot onto a 'Window'
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__draw.htm)
--
-- *Note* X and Y should be vectors.
--
drawPlot2d
  :: Window
  -- ^ is the window handle
  -> Array a
  -- ^ is an 'Array' with the x-axis data points
  -> Array a
  -- ^ is an 'Array' with the y-axis data points
  -> Cell
  -- ^ is structure 'Cell' that has the properties that are used for the current rendering.
  -> IO ()
drawPlot2d (Window w) (Array fptr1) (Array fptr2) cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
    withForeignPtr fptr2 $ \ptr2 ->
     withForeignPtr w $ \wptr ->
      alloca $ \cellPtr -> do
        poke cellPtr =<< cellToAFCell cell
        throwAFError =<< af_draw_plot_2d wptr ptr1 ptr2 cellPtr

-- | Draw a 3D plot onto a 'Window'
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__draw.htm)
--
-- *Note* X, Y and Z should be vectors.
--
drawPlot3d
  :: Window
  -- ^ is the window handle
  -> Array a
  -- ^ is an 'Array' with the x-axis data points
  -> Array a
  -- ^ is an 'Array' with the y-axis data points
    -> Array a
  -- ^ is an 'Array' with the z-axis data points
  -> Cell
  -- ^ is structure 'Cell' that has the properties that are used for the current rendering.
  -> IO ()
drawPlot3d (Window w) (Array fptr1) (Array fptr2) (Array fptr3) cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
    withForeignPtr fptr2 $ \ptr2 ->
      withForeignPtr fptr3 $ \ptr3 ->
        withForeignPtr w $ \wptr ->
          alloca $ \cellPtr -> do
            poke cellPtr =<< cellToAFCell cell
            throwAFError =<< af_draw_plot_3d wptr ptr1 ptr2 ptr3 cellPtr

-- | Draw a scatter plot onto a 'Window'
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__draw.htm)
--
-- *Note* X and Y should be vectors.
--
drawScatter
  :: Window
  -- ^ is the window handle
  -> Array a
  -- ^ is an 'Array' with the x-axis data points
  -> Array a
  -- ^ is an 'Array' with the y-axis data points
  -> MarkerType
  -- ^ enum specifying which marker to use in the scatter plot
  -> Cell
  -- ^ is structure 'Cell' that has the properties that are used for the current rendering.
  -> IO ()
drawScatter (Window w) (Array fptr1) (Array fptr2) (fromMarkerType -> m) cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
    withForeignPtr fptr2 $ \ptr2 ->
     withForeignPtr w $ \wptr ->
      alloca $ \cellPtr -> do
        poke cellPtr =<< cellToAFCell cell
        throwAFError =<< af_draw_scatter wptr ptr1 ptr2 m cellPtr

-- | Draw a scatter plot onto a 'Window'
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__draw.htm#ga764410fbdf0cd60c7044c77e36fb2577)
--
-- *Note* X and Y should be vectors.
--
drawScatter3
  :: Window
  -- ^ is the window handle
  -> Array a
  -- ^ is an af_array or matrix with the xyz-values of the points
  -> MarkerType
  -- ^ is an af_marker_type enum specifying which marker to use in the scatter plot
  -> Cell
  -- ^ is structure af_cell that has the properties that are used for the current rendering.
  -> IO ()
drawScatter3 (Window w) (Array fptr1) (fromMarkerType -> m) cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
   withForeignPtr w $ \wptr ->
    alloca $ \cellPtr -> do
      poke cellPtr =<< cellToAFCell cell
      throwAFError =<< af_draw_scatter3 wptr ptr1 m cellPtr

-- | Draw a scatter plot onto a 'Window'
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__draw.htm#ga9991b93681e0c18693a5464458781d22)
--
-- *Note* in must be 2d and of the form [n, order], where order is either 2 or 3. If order is 2, then chart is 2D and if order is 3, then chart is 3D.
--
drawScatterNd
  :: Window
  -- ^ is the window handle
  -> Array a
  -- ^ is an 'Array' or matrix with the xyz-values of the points
  -> MarkerType
  -- ^ is an af_marker_type enum specifying which marker to use in the scatter plot
  -> Cell
  -- ^ is structure af_cell that has the properties that are used for the current rendering.
  -> IO ()
drawScatterNd (Window w) (Array fptr1) (fromMarkerType -> m) cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
   withForeignPtr w $ \wptr ->
    alloca $ \cellPtr -> do
      poke cellPtr =<< cellToAFCell cell
      throwAFError =<< af_draw_scatter_nd wptr ptr1 m cellPtr

-- | Draw a scatter plot onto a 'Window'
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__draw.htm#ga79417722c69883e7a91282b138288010)
--
-- *Note* in must be 2d and of the form [n, order], where order is either 2 or 3. If order is 2, then chart is 2D and if order is 3, then chart is 3D.
--
drawScatter2d
  :: Window
  -- ^ is the window handle
  -> Array a
  -- ^ is an af_array with the x-axis data points
  -> Array a
  -- ^ is an af_array with the y-axis data points
  -> MarkerType
  -- ^ is an af_marker_type enum specifying which marker to use in the scatter plot
  -> Cell
  -- ^ is structure af_cell that has the properties that are used for the current rendering.
  -> IO ()
drawScatter2d (Window w) (Array fptr1) (Array fptr2) (fromMarkerType -> m) cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
   withForeignPtr w $ \wptr ->
   withForeignPtr fptr2 $ \ptr2 ->
    alloca $ \cellPtr -> do
      poke cellPtr =<< cellToAFCell cell
      throwAFError =<< af_draw_scatter_2d wptr ptr1 ptr2 m cellPtr

-- | Draw a scatter plot onto a 'Window'
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__draw.htm#ga2b3d0dd690ebcba4c4dbb09cdcaed304)
--
-- *Note* X, Y and Z should be vectors.
--
drawScatter3d
  :: Window
  -- ^ is the window handle
  -> Array a
  -- ^ is an af_array with the x-axis data points
  -> Array a
  -- ^ is an af_array with the y-axis data points
  -> Array a
  -- ^ is an af_array with the z-axis data points
  -> MarkerType
  -- ^ is an af_marker_type enum specifying which marker to use in the scatter plot
  -> Cell
  -- ^ is structure af_cell that has the properties that are used for the current rendering.
  -> IO ()
drawScatter3d (Window w) (Array fptr1) (Array fptr2) (Array fptr3) (fromMarkerType -> m) cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
   withForeignPtr w $ \wptr ->
   withForeignPtr fptr2 $ \ptr2 ->
    withForeignPtr fptr3 $ \ptr3 ->
      alloca $ \cellPtr -> do
        poke cellPtr =<< cellToAFCell cell
        throwAFError =<< af_draw_scatter_3d wptr ptr1 ptr2 ptr3 m cellPtr

-- | Draw a Histogram onto a 'Window'
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__draw.htm#gaf1648ee35739c86116bfa9c22644dbd7)
--
-- *Note* X should be a vector.
--
drawHistogram
  :: Window
  -- ^ is the window handle
  -> Array a
  -- ^ is the data frequency af_array
  -> Double
  -- ^ is the value of the minimum data point of the array whose histogram(X) is going to be rendered.
  -> Double
  -- ^ is the value of the maximum data point of the array whose histogram(X) is going to be rendered.
  -> Cell
  -- ^ is structure 'Cell' that has the properties that are used for the current rendering.
  -> IO ()
drawHistogram (Window w) (Array fptr1) minval maxval cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
   withForeignPtr w $ \wptr ->
    alloca $ \cellPtr -> do
      poke cellPtr =<< cellToAFCell cell
      throwAFError =<< af_draw_hist wptr ptr1 minval maxval cellPtr

-- | Draw a Surface onto a 'Window'
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__draw.htm#gaaee14e457272b2cd1bd4ed1228370229)
--
-- *Note* X and Y should be vectors. S should be a 2D array
--
drawSurface
  :: Window
  -- ^ is the window handle
  -> Array a
  -- ^ is an af_array with the x-axis data points
  -> Array a
  -- ^ is an af_array with the y-axis data points
  -> Array a
  -- ^ is an af_array with the z-axis data points
  -> Cell
  -- ^ is structure af_cell that has the properties that are used for the current rendering.
  -> IO ()
drawSurface (Window w) (Array fptr1) (Array fptr2) (Array fptr3) cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
   withForeignPtr w $ \wptr ->
   withForeignPtr fptr2 $ \ptr2 ->
    withForeignPtr fptr3 $ \ptr3 ->
      alloca $ \cellPtr -> do
        poke cellPtr =<< cellToAFCell cell
        throwAFError =<< af_draw_surface wptr ptr1 ptr2 ptr3 cellPtr

-- | Draw a Vector Field onto a 'Window'
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__draw.htm#ga2d31a148578d749be4224e7119b386bc)
--
-- *Note* all the 'Array' inputs should be vectors and the same size
--
drawVectorFieldND
  :: Window
  -- ^ is the window handle
  -> Array a
  -- ^ is an 'Array' with the points
  -> Array a
  -- ^ is an 'Array' with the directions
  -> Cell
  -- ^ is structure 'Cell' that has the properties that are used for the current rendering.
  -> IO ()
drawVectorFieldND (Window w) (Array fptr1) (Array fptr2) cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
   withForeignPtr fptr2 $ \ptr2 ->
     withForeignPtr w $ \wptr ->
      alloca $ \cellPtr -> do
        poke cellPtr =<< cellToAFCell cell
        throwAFError =<< af_draw_vector_field_nd wptr ptr1 ptr2 cellPtr

-- | Draw a Vector Field onto a 'Window'
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__draw.htm#gaf2d3be32c1b6a9034a3bb851206b4b5a)
--
-- *Note* all the 'Array' inputs should be vectors and the same size
--
drawVectorField3d
  :: Window
  -- ^ is the window handle
  -> Array a
  -- ^ is an 'Array' with the x-axis points
  -> Array a
  -- ^ is an 'Array' with the y-axis points
  -> Array a
  -- ^ is an 'Array' with the z-axis points
  -> Array a
  -- ^ is an 'Array' with the x-axis directions
  -> Array a
  -- ^ is an 'Array' with the y-axis directions
  -> Array a
  -- ^ is an 'Array' with the z-axis directions
  -> Cell
  -- ^ is structure 'Cell' that has the properties that are used for the current rendering.
  -> IO ()
drawVectorField3d (Window w) (Array fptr1) (Array fptr2) (Array fptr3)
  (Array fptr4) (Array fptr5) (Array fptr6) cell =
  mask_ $ do
   withForeignPtr w $ \wptr ->
    withForeignPtr fptr1 $ \ptr1 ->
      withForeignPtr fptr2 $ \ptr2 ->
        withForeignPtr fptr3 $ \ptr3 ->
          withForeignPtr fptr4 $ \ptr4 ->
            withForeignPtr fptr5 $ \ptr5 ->
              withForeignPtr fptr6 $ \ptr6 -> do
                alloca $ \cellPtr -> do
                  poke cellPtr =<< cellToAFCell cell
                  throwAFError =<< af_draw_vector_field_3d wptr ptr1 ptr2 ptr3 ptr4 ptr5 ptr6 cellPtr

-- | Draw a Vector Field onto a 'Window'
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__draw.htm#gaa1a667e4d29ab089629acd5296f29a7b)
--
-- *Note* all the 'Array' inputs should be vectors and the same size
--
drawVectorField2d
  :: Window
  -- ^ is the window handle
  -> Array a
  -- ^ is an 'Array' with the x-axis points
  -> Array a
  -- ^ is the window handle
  -> Array a
  -- ^ is the window handle
  -> Array a
  -- ^ is the window handle
  -> Cell
  -- ^ is the window handle
  -> IO ()
drawVectorField2d (Window w) (Array fptr1) (Array fptr2) (Array fptr3) (Array fptr4) cell =
  mask_ $ do
   withForeignPtr w $ \wptr ->
    withForeignPtr fptr1 $ \ptr1 ->
      withForeignPtr fptr2 $ \ptr2 ->
        withForeignPtr fptr3 $ \ptr3 ->
          withForeignPtr fptr4 $ \ptr4 ->
            alloca $ \cellPtr -> do
              poke cellPtr =<< cellToAFCell cell
              throwAFError =<< af_draw_vector_field_2d wptr ptr1 ptr2 ptr3 ptr4 cellPtr

-- | Draw a grid onto a 'Window'
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__window.htm#ga37fc7eb00ae11c25e1a60d341663d68d)
--
-- *Note* all the 'Array' inputs should be vectors and the same size
--
grid
  :: Window
  -- ^ is the window handle
  -> Int
  -- ^ is number of rows you want to show in a window
  -> Int
  -- ^ is number of coloumns you want to show in a window
  -> IO ()
grid (Window w) (fromIntegral -> rows) (fromIntegral -> cols) =
  mask_ . withForeignPtr w $ \wptr ->
    throwAFError =<< af_grid wptr rows cols

-- | Setting axes limits for a histogram/plot/surface/vector field.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__window.htm#ga62d2cad30e3aad06c24999fe5ac34598)
--
-- *Note* Set to NULL if the chart is 2D.
--
setAxesLimitsCompute
  :: Window
  -- ^ is the window handle
  -> Array a
  -- ^ the data to compute the limits for x-axis.
  -> Array a
  -- ^ the data to compute the limits for y-axis.
  -> Array a
  -- ^ the data to compute the limits for z-axis.
  -> Bool
  -- ^ is for using the exact min/max values from x, y and z. If exact is false then the most significant digit is rounded up to next power of 2 and the magnitude remains the same.
  -> Cell
  -- ^ is structure 'Cell' that has the properties that are used for the current rendering.
  -> IO ()
setAxesLimitsCompute (Window w) (Array fptr1) (Array fptr2) (Array fptr3) (fromIntegral . fromEnum -> exact) cell =
  mask_ $ do
   withForeignPtr w $ \wptr ->
    withForeignPtr fptr1 $ \ptr1 ->
      withForeignPtr fptr2 $ \ptr2 ->
        withForeignPtr fptr3 $ \ptr3 ->
          alloca $ \cellPtr -> do
            poke cellPtr =<< cellToAFCell cell
            throwAFError =<< af_set_axes_limits_compute wptr ptr1 ptr2 ptr3 exact cellPtr

-- | Setting axes limits for a 2D histogram/plot/surface/vector field.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__window.htm#gadadc41caf7d6a9b7ca2e674079971895)
--
setAxesLimits2d
  :: Window
  -- ^ is the window handle
  -> Float
  -- ^ is the minimum on x-axis
  -> Float
  -- ^ is the maximum on x-axis
  -> Float
  -- ^ is the minimum on y-axis
  -> Float
  -- ^ is the maximum on y-axis
  -> Bool
  -- ^ is for using the exact min/max values from x, and y. If exact is false then the most significant digit is rounded up to next power of 2 and the magnitude remains the same.
  -> Cell
  -- ^ is structure af_cell that has the properties that are used for the current rendering.
  -> IO ()
setAxesLimits2d (Window w) xmin xmax ymin ymax (fromIntegral . fromEnum -> exact) cell =
  mask_ $ do
   withForeignPtr w $ \wptr ->
    alloca $ \cellPtr -> do
      poke cellPtr =<< cellToAFCell cell
      throwAFError =<< af_set_axes_limits_2d wptr xmin xmax ymin ymax exact cellPtr

-- | Setting axes limits for a 3D histogram/plot/surface/vector field.
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__window.htm#gadcd1bd46b9d6fabc047365ca5dc3f73d)
--
setAxesLimits3d
  :: Window
  -- ^ is the window handle
  -> Float
  -- ^ is the minimum on x-axis
  -> Float
  -- ^ is the maximum on x-axis
  -> Float
  -- ^ is the minimum on y-axis
  -> Float
  -- ^ is the maximum on y-axis
  -> Float
  -- ^ is the minimum on z-axis
  -> Float
  -- ^ is the maximum on z-axis
  -> Bool
  -- ^ is for using the exact min/max values from x, y and z. If exact is false then the most significant digit is rounded up to next power of 2 and the magnitude remains the same.
  -> Cell
  -- ^ is structure 'Cell' that has the properties that are used for the current rendering.
  -> IO ()
setAxesLimits3d (Window w) xmin xmax ymin ymax zmin zmax (fromIntegral . fromEnum -> exact) cell =
  mask_ $ do
   withForeignPtr w $ \wptr ->
    alloca $ \cellPtr -> do
      poke cellPtr =<< cellToAFCell cell
      throwAFError =<< af_set_axes_limits_3d wptr xmin xmax ymin ymax zmin zmax exact cellPtr


-- | Setting axes titles
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__window.htm#gadcd1bd46b9d6fabc047365ca5dc3f73d)
--
setAxesTitles
  :: Window
  -- ^ is the window handle
  -> String
  -- ^ is the name of the x-axis
  -> String
  -- ^ is the name of the y-axis
  -> String
  -- ^ is the name of the z-axis
  -> Cell
  -- ^ is structure 'Cell' that has the properties that are used for the current rendering.
  -> IO ()
setAxesTitles (Window w) x y z cell =
  mask_ $ do
   withForeignPtr w $ \wptr ->
    alloca $ \cellPtr -> do
      withCString x $ \xstr ->
        withCString y $ \ystr ->
          withCString z $ \zstr -> do
            poke cellPtr =<< cellToAFCell cell
            throwAFError =<< af_set_axes_titles wptr xstr ystr zstr cellPtr

-- | Displays 'Window'
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__window.htm#ga50dae861324dca1cce9f583256f5a654)
--
showWindow
  :: Window
  -- ^ 'Window' handle
   -> IO ()
showWindow = (`opw` af_show)

-- | Checks if 'Window' is closed
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__window.htm#ga50dae861324dca1cce9f583256f5a654)
--
isWindowClosed :: Window -> IO Bool
isWindowClosed w =
  toEnum . fromIntegral
    <$> (w `opw1` af_is_window_closed)

-- | Sets 'Window' visibility
--
-- [ArrayFire Docs](http://arrayfire.org/docs/group__gfx__func__window.htm#gad7b63c70d45e101c4d8d500273e310c7)
--
setVisibility
  :: Window
  -- ^ 'Window' handle
  -> Bool
  -- ^ Set to 'True' to display 'Window'
  -> IO ()
setVisibility w (fromIntegral . fromEnum -> b) = w `opw` (`af_set_visibility` b)
