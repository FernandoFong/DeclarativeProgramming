module Trees where

data BTree a = Void | R a (BTree a) (BTree a) deriving Show

insertBST ::Ord a => a -> BTree a -> BTree a
insertBST x Void = R x Void Void
insertBST x (R y t1 t2) = if x <= y
                        then R y (insertBST x t1) t2
                        else R y t1 (insertBST x t2)

creaBST :: Ord a => [a] -> BTree a
creaBST ls = creaBST_aux ls Void
  where creaBST_aux ls t = case ls of
                             [] -> t
                             (y:ys) -> creaBST_aux ys (insertBST y t)

preorder :: BTree a -> [a]
preorder Void = []
preorder (R x t1 t2) = x: (preorder t1)++(preorder t2)

inorder :: BTree a -> [a]
inorder Void = []
inorder (R x t1 t2) = (inorder t1)++[x]++(inorder t2)

postorder :: BTree a -> [a]
postorder Void = []
postorder (R x t1 t2) = (postorder t1)++(postorder t2)++[x]

treeSort :: Ord a => [a] -> [a]
treeSort = inorder . creaBST

sumaIchi :: Int -> Num
sumaIchi n = div (n * 100) 833
