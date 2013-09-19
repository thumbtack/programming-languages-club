import Foreign.C

foreign import ccall unsafe "stdlib.h srand" c_srand :: CULong -> IO ()
foreign import ccall unsafe "stdlib.h rand" c_rand :: IO CLong

main :: IO ()
main = do
    c_srand 123
    random_value <- c_rand
    putStrLn $ show random_value
