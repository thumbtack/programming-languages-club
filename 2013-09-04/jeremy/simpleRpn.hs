type Op = Float -> Float -> Float

main = do
  line <- getLine
  putStrLn $ show $ rpn (words line) []

rpn :: [String] -> [Float] -> Float
rpn []     [n]   = n
rpn (x:xs) stack = case x of
  "+" -> runOp (+) xs stack
  "-" -> runOp (-) xs stack
  "*" -> runOp (*) xs stack
  "/" -> runOp (/) xs stack
  s   -> rpn xs (read s : stack)
rpn a b = error ("Can't continue with inputs " ++ show a ++ " and " ++ show b)

runOp :: Op -> [String] -> [Float] -> Float
runOp op rest (right:left:stack) = rpn rest (op left right : stack)
runOp _  _    _                  = error "Too many operators"
