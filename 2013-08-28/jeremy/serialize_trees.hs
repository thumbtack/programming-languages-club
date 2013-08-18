
data ArbitraryTree a = Node a [(ArbitraryTree a)] | EmptyTree
                     deriving (Show)

intersperse :: a -> [a] -> [a]
intersperse a (x:y:zs) = x : a : intersperse a (y:zs)
intersperse a lst = lst

serialize :: (Show a) => (ArbitraryTree a) -> String
serialize EmptyTree         = "()"
serialize (Node x children) = "(" ++ show x ++ " " ++ inner ++ ")"
  where inner = foldl (++) "" $ intersperse " " $ map serialize children

main = putStrLn $ serialize (Node "c" [EmptyTree, Node "d" [], Node "e" []])
