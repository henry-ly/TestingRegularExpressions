{-# LANGUAGE ForeignFunctionInterface #-}
module FFI where

-- note to self: ghci ffi.hs simple_re_match.o

import Foreign.C.Types 
import Foreign.C.String
import Foreign.Ptr 
import Foreign.Marshal.Array 
import System.IO.Unsafe 
foreign import ccall "match"
     c_match :: CString -> CString -> CInt

matcher :: String -> String -> IO CInt
matcher a b = do 
              newRegex <- newCString a
              newString <- newCString b
              return $ c_match newRegex newString

