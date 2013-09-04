data Tree a = Node {value :: a, children :: [Tree a]}
    deriving (Show, Read)

serialize :: (Show a) => Tree a -> String
serialize x = svalue ++ " " ++ slength ++ " " ++ schildren
    where svalue = show $ value x
          slength = show $ length $ children x
          serialize_list xs = foldl (\ x y -> x ++ serialize y) "" xs
          schildren = serialize_list $ children x

-- usage: deserialize "10 2 3 0 2 1 4 0" :: Tree Int
deserialize :: (Read a) => String -> Tree a
deserialize x = head $ fst $ deserialize_list (words x) 1

-- usage: deserialize_list ["10", "2", "3", "0", "2", "1", "4", "0"] 1 :: ([Tree Int], [String])
deserialize_list :: (Read a) => [String] -> Int -> ([Tree a], [String])
deserialize_list xs 0 = ([], xs)
deserialize_list (v:l:xs) y = (node:nodes, unprocessed)
    where value = read v
          length = (read l)::Int
          (child_nodes, rest) = deserialize_list xs length
          node = Node {value=value, children=child_nodes}
          (nodes, unprocessed) = deserialize_list rest (y-1)

main = do
    putStrLn $ serialize $ (deserialize $ serialize $ (deserialize "10 2 3 0 2 1 4 0" :: Tree Int):: Tree Int)
    putStrLn $ show $ (deserialize $ serialize (Node {value=10, children=[Node {value=3, children=[]}, Node {value=2, children=[Node {value=4, children=[]}]}]}) :: Tree Int)
