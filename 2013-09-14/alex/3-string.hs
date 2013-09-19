import Foreign.C
import Foreign.C.String

foreign import ccall unsafe "stdio.h puts" c_puts :: CString -> IO ()

main :: IO ()
main = hs_puts "Hello!"

-- withCString :: String -> (CString -> IO a) -> IO a
hs_puts :: String -> IO ()
hs_puts s = withCString s $ \cs -> c_puts cs
