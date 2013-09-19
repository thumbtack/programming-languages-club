import Foreign.C

foreign import ccall unsafe "math.h sin" c_sin :: CDouble -> CDouble

hs_sin :: Double -> Double
hs_sin = realToFrac . c_sin . realToFrac

main :: IO ()
main = putStrLn $ show $ hs_sin 1.23
