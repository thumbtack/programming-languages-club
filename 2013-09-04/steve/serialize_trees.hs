data TreeNode a = None | Node {value :: a, left :: TreeNode a, right :: TreeNode a} deriving (Show)

serialize_helper :: (Show a) => TreeNode a -> [String] -> [String]
serialize_helper None accumulator = ".":accumulator
serialize_helper node accumulator =
  serialize_helper (right node) $ serialize_helper (left node) $ (show $ value node):accumulator

serialize :: (Show a) => TreeNode a -> String
serialize node = unwords $ reverse $ serialize_helper node []

deserialize_helper :: (Read a) => [String] -> (TreeNode a, [String])
deserialize_helper (".":rest_of_input) = (None, rest_of_input)
deserialize_helper (node_value:rest_of_input) =
  let
    (left_subtree, input_after_left) = deserialize_helper rest_of_input
    (right_subtree, input_after_right) = deserialize_helper input_after_left
  in
   (Node {value=(read node_value), left=left_subtree, right=right_subtree}, input_after_right)

deserialize :: (Read a) => String -> TreeNode a
deserialize = fst . deserialize_helper . words