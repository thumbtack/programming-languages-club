data OperatorType = Plus | Minus | Times | Divide deriving (Show)
data Token = Value Float | Operator OperatorType deriving (Show)
data RpnResult = Result Float | Error String

parse_token :: String -> Token
parse_token string =
  case string of
    "+" -> Operator Plus
    "-" -> Operator Minus
    "*" -> Operator Times
    "/" -> Operator Divide
    otherwise -> Value $ read string

parse_rpn_strings :: [String] -> [Token] -> [Token]
parse_rpn_strings [] accumulator = reverse accumulator
parse_rpn_strings (next_string:rest_of_strings) accumulator =
  parse_rpn_strings rest_of_strings $ (parse_token next_string):accumulator

evaluate_operator :: OperatorType -> Float -> Float -> Float
evaluate_operator op lhs rhs =
  case op of
    Plus -> lhs + rhs
    Minus -> lhs - rhs
    Times -> lhs * rhs
    Divide -> lhs / rhs

calculate_rpn_on_list :: [Token] -> [Token] -> RpnResult
calculate_rpn_on_list ((Value final_value):[]) [] = Result final_value
calculate_rpn_on_list ((Value rhs):(Value lhs):rest_of_stack) ((Operator op):rest_of_input) =
  calculate_rpn_on_list ((Value $ evaluate_operator op lhs rhs):rest_of_stack) rest_of_input
calculate_rpn_on_list stack ((Value next_value):rest_of_input) =
  calculate_rpn_on_list ((Value next_value):stack) rest_of_input
calculate_rpn_on_list stack input =
  Error $ "Invalid input, stack is " ++ (show stack) ++ ", remaining input is " ++ (show input)

calculate_rpn :: String -> RpnResult
calculate_rpn string = calculate_rpn_on_list [] $ parse_rpn_strings (words string) []

main = do
  input <- getLine
  case (calculate_rpn input) of
    Result value -> putStrLn $ "I got " ++ (show value) ++ ", what did you get?"
    Error message -> putStrLn $ "Uh oh, " ++ message