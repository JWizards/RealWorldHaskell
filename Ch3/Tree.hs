data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving Show


treeVal (Node val _ _) = Just val
treeVal _ = Nothing


-- alternate tree defn
data TreeAlt a = Node a (Maybe (Tree a)) (Maybe (Tree a)) deriving Show
