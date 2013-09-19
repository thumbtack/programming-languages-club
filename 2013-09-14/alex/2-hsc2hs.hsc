import Foreign.C

#include "limits.h"

main :: IO ()
main = do
    putStrLn $ show #{const CHAR_MAX}
    putStrLn $ show #{size double}
