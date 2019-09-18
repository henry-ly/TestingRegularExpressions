{-# LANGUAGE ForeignFunctionInterface #-}
module FFI where

{- In order to create a working executable
gcc simple_re_match.c -o simple_re_match 
ghc --make -main-is FFI ffi.hs simple_re_match.c
note to self: ghci ffi.hs simple_re_match.o
-}

import Foreign.C.Types 
import Foreign.C.String
import Foreign.Ptr 
import Foreign.Marshal.Array 
import System.IO.Unsafe 
foreign import ccall "match"
     c_match :: CString -> CString -> CInt
match :: CString -> CString -> CInt
match a b = c_match a b

matcher :: String -> String -> IO CInt
matcher a b = do 
              newRegex <- newCString a
              newString <- newCString b
              return $ match newRegex newString


main = do 
  newC <- newCString "Hello"
  putStrLn $ show(match newC newC)
  return()
