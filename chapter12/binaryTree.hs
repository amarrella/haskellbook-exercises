data BinaryTree a = 
    Leaf
  | Node (BinaryTree a) a (BinaryTree a) 
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a))
       -> a
       -> BinaryTree b
unfold f a =
  case f a of
    Just (a1, b1, a2) -> 
      Node (unfold f a1) b1 (unfold f a2)
    Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n =
  unfold (\a -> if(a == n) then Nothing else Just(a+1,a,a+1)) 0