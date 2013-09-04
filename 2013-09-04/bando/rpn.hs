import qualified Data.Map as Map

main = do
    input <- getLine
    putStrLn $ show $ rpn (words input) []

rpn :: (Fractional a, Read a) => [String] -> [a] -> a
rpn [] [x] = x
rpn (x:xs) stack@(right:left:rest)
    | x `Map.member` operators = rpn xs $ ((operators Map.! x) left right):rest
    | otherwise                = rpn xs ((read x):stack)
    where operators = Map.fromList [("+", (+)), ("-", (-)), ("*", (*)), ("/", (/))]
rpn (x:xs) stack = rpn xs ((read x):stack)
rpn _ _ = error "Bad input"
