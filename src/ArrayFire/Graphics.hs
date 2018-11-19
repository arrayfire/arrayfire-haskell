module ArrayFire.Graphics where

import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String

-- import ArrayFire.Internal.Util
import ArrayFire.Internal.Graphics
import ArrayFire.Exception
import ArrayFire.Internal.Defines

    -- /**
    --     \param[out] arr is the generated array of given type
    --     \param[in] val is the value of each element in the generated array
    --     \param[in] ndims is size of dimension array \p dims
    --     \param[in] dims is the array containing sizes of the dimension
    --     \param[in] type is the type of array to generate
    --    \ingroup data_func_constant
    -- /

createWindow :: Int -> Int -> String -> IO AFWindow
createWindow x y msg = do
  alloca $ \ptr -> do
    print =<< af_create_window ptr x y =<< newCString msg
    peek ptr

showWindow :: AFWindow -> IO ()
showWindow win = print =<< af_show win

setWindowVisibility :: AFWindow -> Bool -> IO ()
setWindowVisibility win isOpen =
  print =<< af_set_visibility win isOpen
