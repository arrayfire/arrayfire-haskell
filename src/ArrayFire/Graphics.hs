{-# LANGUAGE ViewPatterns #-}
module ArrayFire.Graphics where

import Control.Exception
import Foreign.Marshal
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.C.String

import ArrayFire.Internal.Graphics
import ArrayFire.Exception
import ArrayFire.FFI
import ArrayFire.Types
import ArrayFire.Internal.Defines

createWindow :: Int -> Int -> String -> IO Window
createWindow x y str =
  withCString str $ \cstr ->
    createWindow' (\p -> af_create_window p x y cstr)

setPosition :: Window -> Int -> Int -> IO ()
setPosition w (fromIntegral -> x) (fromIntegral -> y) =
  w `opw` (\p -> af_set_position p x y)

setTitle :: Window -> String -> IO ()
setTitle w str = withCString str $ \cstr ->
  w `opw` (\p -> af_set_title p cstr)

setSize :: Window -> Int -> Int -> IO ()
setSize w (fromIntegral -> x) (fromIntegral -> y) =
  w `opw` (\p -> af_set_size p x y)

drawImage :: Window -> Array a -> Cell -> IO ()
drawImage (Window wfptr) (Array fptr) cell =
  mask_ $ withForeignPtr fptr $ \aptr ->
    withForeignPtr wfptr $ \wptr ->
      alloca $ \cellPtr -> do
        poke cellPtr =<< cellToAFCell cell
        throwAFError =<< af_draw_image wptr aptr cellPtr
        free cellPtr

drawPlot :: Window -> Array a -> Array a -> Cell -> IO ()
drawPlot (Window w) (Array fptr1) (Array fptr2) cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
    withForeignPtr fptr2 $ \ptr2 ->
      withForeignPtr w $ \wptr ->
      alloca $ \cellPtr -> do
        poke cellPtr =<< cellToAFCell cell
        throwAFError =<< af_draw_plot wptr ptr1 ptr2 cellPtr
        free cellPtr

drawPlot3 :: Window -> Array a -> Cell -> IO ()
drawPlot3 (Window w) (Array fptr) cell =
  mask_ $ withForeignPtr fptr $ \aptr ->
    withForeignPtr w $ \wptr ->
    alloca $ \cellPtr -> do
      poke cellPtr =<< cellToAFCell cell
      throwAFError =<< af_draw_plot3 wptr aptr cellPtr
      free cellPtr

drawPlotNd :: Window -> Array a -> Cell -> IO ()
drawPlotNd (Window w) (Array fptr) cell =
  mask_ $ withForeignPtr fptr $ \aptr ->
    withForeignPtr w $ \wptr ->
    alloca $ \cellPtr -> do
      poke cellPtr =<< cellToAFCell cell
      throwAFError =<< af_draw_plot_nd wptr aptr cellPtr
      free cellPtr

drawPlot2d :: Window -> Array a -> Array a -> Cell -> IO ()
drawPlot2d (Window w) (Array fptr1) (Array fptr2) cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
    withForeignPtr fptr2 $ \ptr2 ->
     withForeignPtr w $ \wptr ->
      alloca $ \cellPtr -> do
        poke cellPtr =<< cellToAFCell cell
        throwAFError =<< af_draw_plot_2d wptr ptr1 ptr2 cellPtr
        free cellPtr

drawPlot3d :: Window -> Array a -> Array a -> Array a -> Cell -> IO ()
drawPlot3d (Window w) (Array fptr1) (Array fptr2) (Array fptr3) cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
    withForeignPtr fptr2 $ \ptr2 ->
     withForeignPtr fptr3 $ \ptr3 ->
     withForeignPtr w $ \wptr ->
      alloca $ \cellPtr -> do
        poke cellPtr =<< cellToAFCell cell
        throwAFError =<< af_draw_plot_3d wptr ptr1 ptr2 ptr2 cellPtr
        free cellPtr

drawScatter :: Window -> Array a -> Array a -> MarkerType -> Cell -> IO ()
drawScatter (Window w) (Array fptr1) (Array fptr2) (fromMarkerType -> m) cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
    withForeignPtr fptr2 $ \ptr2 ->
     withForeignPtr w $ \wptr ->
      alloca $ \cellPtr -> do
        poke cellPtr =<< cellToAFCell cell
        throwAFError =<< af_draw_scatter wptr ptr1 ptr2 m cellPtr
        free cellPtr

drawScatter3 :: Window -> Array a -> MarkerType -> Cell -> IO ()
drawScatter3 (Window w) (Array fptr1) (fromMarkerType -> m) cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
   withForeignPtr w $ \wptr ->
    alloca $ \cellPtr -> do
      poke cellPtr =<< cellToAFCell cell
      throwAFError =<< af_draw_scatter3 wptr ptr1 m cellPtr
      free cellPtr

drawScatterNd :: Window -> Array a -> MarkerType -> Cell -> IO ()
drawScatterNd (Window w) (Array fptr1) (fromMarkerType -> m) cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
   withForeignPtr w $ \wptr ->
    alloca $ \cellPtr -> do
      poke cellPtr =<< cellToAFCell cell
      throwAFError =<< af_draw_scatter_nd wptr ptr1 m cellPtr
      free cellPtr

drawScatter2d :: Window -> Array a -> Array a -> MarkerType -> Cell -> IO ()
drawScatter2d (Window w) (Array fptr1) (Array fptr2) (fromMarkerType -> m) cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
   withForeignPtr w $ \wptr ->
   withForeignPtr fptr2 $ \ptr2 ->
    alloca $ \cellPtr -> do
      poke cellPtr =<< cellToAFCell cell
      throwAFError =<< af_draw_scatter_2d wptr ptr1 ptr2 m cellPtr
      free cellPtr

drawScatter3d :: Window -> Array a -> Array a -> Array a -> MarkerType -> Cell -> IO ()
drawScatter3d (Window w) (Array fptr1) (Array fptr2) (Array fptr3) (fromMarkerType -> m) cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
   withForeignPtr w $ \wptr ->
   withForeignPtr fptr2 $ \ptr2 ->
    withForeignPtr fptr3 $ \ptr3 ->
      alloca $ \cellPtr -> do
        poke cellPtr =<< cellToAFCell cell
        throwAFError =<< af_draw_scatter_3d wptr ptr1 ptr2 ptr3 m cellPtr
        free cellPtr

drawHistogram :: Window -> Array a -> Double -> Double -> Cell -> IO ()
drawHistogram (Window w) (Array fptr1) minval maxval cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
   withForeignPtr w $ \wptr ->
    alloca $ \cellPtr -> do
      poke cellPtr =<< cellToAFCell cell
      throwAFError =<< af_draw_hist wptr ptr1 minval maxval cellPtr
      free cellPtr

drawSurface :: Window -> Array a -> Array a -> Array a -> Cell -> IO ()
drawSurface (Window w) (Array fptr1) (Array fptr2) (Array fptr3) cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
   withForeignPtr w $ \wptr ->
   withForeignPtr fptr2 $ \ptr2 ->
    withForeignPtr fptr3 $ \ptr3 ->
      alloca $ \cellPtr -> do
        poke cellPtr =<< cellToAFCell cell
        throwAFError =<< af_draw_surface wptr ptr1 ptr2 ptr3 cellPtr
        free cellPtr

drawVectorFieldND :: Window -> Array a -> Array a -> Cell -> IO ()
drawVectorFieldND (Window w) (Array fptr1) (Array fptr2) cell =
  mask_ $ withForeignPtr fptr1 $ \ptr1 ->
   withForeignPtr fptr2 $ \ptr2 ->
     withForeignPtr w $ \wptr ->
      alloca $ \cellPtr -> do
        poke cellPtr =<< cellToAFCell cell
        throwAFError =<< af_draw_vector_field_nd wptr ptr1 ptr2 cellPtr
        free cellPtr

drawVectorField3d :: Window -> Array a -> Array a -> Array a -> Array a -> Array a -> Array a -> Cell -> IO ()
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
                  free cellPtr

drawVectorField2d :: Window -> Array a -> Array a -> Array a -> Array a -> Cell -> IO ()
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
              free cellPtr

grid :: Window -> Int -> Int -> IO ()
grid (Window w) rows cols =
  mask_ . withForeignPtr w $ \wptr ->
    throwAFError =<< af_grid wptr rows cols

setAxesLimitsCompute :: Window -> Array a -> Array a -> Array a -> Bool -> Cell -> IO ()
setAxesLimitsCompute (Window w) (Array fptr1) (Array fptr2) (Array fptr3) exact cell =
  mask_ $ do
   withForeignPtr w $ \wptr ->
    withForeignPtr fptr1 $ \ptr1 ->
      withForeignPtr fptr2 $ \ptr2 ->
        withForeignPtr fptr3 $ \ptr3 ->
          alloca $ \cellPtr -> do
            poke cellPtr =<< cellToAFCell cell
            throwAFError =<< af_set_axes_limits_compute wptr ptr1 ptr2 ptr3 exact cellPtr
            free cellPtr

setAxesLimits2d :: Window -> Float -> Float -> Float -> Float -> Bool -> Cell -> IO ()
setAxesLimits2d (Window w) xmin xmax ymin ymax exact cell =
  mask_ $ do
   withForeignPtr w $ \wptr ->
    alloca $ \cellPtr -> do
      poke cellPtr =<< cellToAFCell cell
      throwAFError =<< af_set_axes_limits_2d wptr xmin xmax ymin ymax exact cellPtr
      free cellPtr

setAxesLimits3d :: Window -> Float -> Float -> Float -> Float -> Float -> Float -> Bool -> Cell -> IO ()
setAxesLimits3d (Window w) xmin xmax ymin ymax zmin zmax exact cell =
  mask_ $ do
   withForeignPtr w $ \wptr ->
    alloca $ \cellPtr -> do
      poke cellPtr =<< cellToAFCell cell
      throwAFError =<< af_set_axes_limits_3d wptr xmin xmax ymin ymax zmin zmax exact cellPtr
      free cellPtr


setAxesTitles :: Window -> String -> String -> String -> Cell -> IO ()
setAxesTitles (Window w) x y z cell =
  mask_ $ do
   withForeignPtr w $ \wptr ->
    alloca $ \cellPtr -> do
      withCString x $ \xstr ->
        withCString y $ \ystr ->
          withCString z $ \zstr -> do
            poke cellPtr =<< cellToAFCell cell
            throwAFError =<< af_set_axes_titles wptr xstr ystr zstr cellPtr
            free cellPtr

showWindow :: Window -> IO ()
showWindow = (`opw` af_show)

isWindowClosed :: Window -> IO Bool
isWindowClosed = (`opw1` af_is_window_closed)

setVisibility :: Window -> Bool -> IO ()
setVisibility w b = w `opw` (`af_set_visibility` b)

