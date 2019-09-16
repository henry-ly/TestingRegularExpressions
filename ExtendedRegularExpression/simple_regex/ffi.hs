{-# LANGUAGE ForeignFunctionInterface #-}
module FFI where
-- run ghc --make -main-is FFI ffi.hs simple_re_match.c
import Foreign.C.Types 
import Foreign.C.String
import Foreign.Ptr 
import Foreign.Marshal.Array 
import System.IO.Unsafe 
foreign import ccall "match"
     c_match :: CString -> CString -> CInt
match :: CString -> CString -> CInt
match a b = c_match a b
main = do 
  newC <- newCString "Hello"
  putStrLn $ show(match newC newC)
  return()
