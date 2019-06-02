module ArrayFire.Graphics where

import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String

import ArrayFire.Internal.Graphics
import ArrayFire.Exception
import ArrayFire.FFI
import ArrayFire.Internal.Defines

createWindow :: Int -> Int -> String -> IO AFWindow
createWindow x y str = do
  cstr <- newCString str
  afCall1 $ \window ->
    af_create_window window x y cstr

showWindow :: AFWindow -> IO ()
showWindow = afCall . af_show

setWindowVisibility :: AFWindow -> Bool -> IO ()
setWindowVisibility win isOpen =
  afCall (af_set_visibility win isOpen)
