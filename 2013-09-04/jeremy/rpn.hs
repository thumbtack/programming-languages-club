
import Control.Monad (liftM, mapM)
import Text.Read (readMaybe)

type Op = (Float -> Float -> Float)
data RPNValue = Operator Op | Value Float

parseOp :: String -> Maybe RPNValue
parseOp s = case s of
  "+" -> makeOp (+)
  "-" -> makeOp (-)
  "*" -> makeOp (*)
  "/" -> makeOp (/)
  _   -> Nothing
  where makeOp = Just . Operator

parseValue :: String -> Maybe RPNValue
parseValue = (liftM Value) . readMaybe

tryAll :: [(a -> Maybe b)] -> a -> Maybe b
tryAll []     _ = Nothing
tryAll (f:fs) v = case f v of
  Nothing -> tryAll fs v
  result  -> result

parseRPNValue :: String -> Either String RPNValue
parseRPNValue s = case tryAll [parseOp, parseValue] s of
  Nothing -> Left ("Can't parse " ++ s)
  Just v  -> Right v

parseRPN :: String -> Either String [RPNValue]
parseRPN s = mapM parseRPNValue (words s)

executeRPN :: [RPNValue] -> [Float] -> Either String Float
executeRPN []                []       = Left "No input"
executeRPN []                [n]      = Right n
executeRPN ((Value v):vs)    stack    = executeRPN vs (v : stack)
executeRPN ((Operator o):os) (r:l:st) = executeRPN os (o l r : st)
executeRPN []                _        = Left "Too many values"
executeRPN _                 _        = Left "Too many operators"

rpn :: String -> Either String Float
rpn s = do
  parsed <- parseRPN s
  executeRPN parsed []

runRpn :: String -> String
runRpn s = case rpn s of
  Left err  -> "Error: " ++ err
  Right val -> show val

main :: IO ()
main = do
  line <- getLine
  putStrLn (runRpn line)
